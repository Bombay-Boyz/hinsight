-- |
-- Module      : HInsight.Ghc.Session
-- Description : Run one GHC typecheck of a file and collect what it reports.
--
-- This is the only module that starts GHC. Foreign exceptions are caught here
-- and converted into 'InsightError' immediately (Standard 1.6); nothing above
-- this module sees a GHC exception.
--
-- The file is analysed together with the modules it imports from its own
-- project: the source folder is worked out from the file's header and place
-- ("HInsight.Project"), its dependencies are typechecked first, and a
-- dependency that does not compile is reported as 'DependencyFailed' instead
-- of analysing the file against broken imports. Every analysis runs in a
-- fresh GHC session, so nothing is carried over from an earlier edit.
--
-- Limitation of this version: packages are not resolved, so the imports that
-- are not the project's own must be installed.
module HInsight.Ghc.Session
  ( ghcInsight,
  )
where

import Control.Exception (Handler (..), IOException, catches, evaluate)
import Control.Monad (filterM)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.Containers.ListUtils (nubOrd)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC
  ( DynFlags (..),
    Ghc,
    GhcLink (NoLink),
    LoadHowMuch (..),
    ModLocation (..),
    ModSummary (..),
    ModuleGraph,
    ParsedModule (..),
    SuccessFlag (..),
    TypecheckedModule (..),
    depanal,
    getSessionDynFlags,
    guessTarget,
    handleSourceError,
    load,
    mgModSummaries,
    noBackend,
    parseModule,
    runGhc,
    setSessionDynFlags,
    setTargets,
    typecheckModule,
  )
import GHC.Data.Bag (bagToList)
import GHC.Driver.DynFlags (gopt_set)
import GHC.Driver.Env.Types (HscEnv (..))
import GHC.Driver.Errors.Types (GhcMessage)
import GHC.Driver.Flags (GeneralFlag (..))
import GHC.Driver.Make (depanalPartial)
import GHC.Driver.Monad (modifySession, pushLogHookM)
import GHC.Driver.Plugins
  ( Plugin (..),
    PluginWithArgs (..),
    Plugins (..),
    StaticPlugin (..),
    defaultPlugin,
  )
import GHC.Hs (HsModule (..))
import GHC.Types.Error (MessageClass (..), MsgEnvelope (..), Severity (..), getMessages)
import GHC.Types.SourceError (SourceError, srcErrorMessages)
import GHC.Types.SrcLoc (unLoc)
import GHC.Unit.Module (moduleNameString)
import GHC.Unit.Module.ModSummary (ms_mod_name)
import GHC.Utils.Logger (LogAction)
import GHC.Utils.Outputable (ppr, (<+>))
import GHC.Utils.Panic (GhcException)
import HInsight.Analysis (Analysis (..), Insight (..))
import HInsight.Config (LibDir, libDirPath)
import HInsight.Error (InsightError (..), SessionError (..))
import HInsight.Flow (Pipeline)
import HInsight.Ghc.Bindings (Binding, collectBindings)
import HInsight.Ghc.Convert (renderGhc)
import HInsight.Ghc.Flow (RawPipeline, collect, pipelines)
import HInsight.Ghc.HoleCapture (CapturedHole, holePlugin)
import HInsight.Ghc.Holes (holeReports)
import HInsight.Ghc.Messages (Extracted (..), extractMessages)
import HInsight.Hole (HoleReport)
import HInsight.Project (sourceRoot)
import HInsight.Source (SourceFile, sourceFilePath)
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist)
import System.FilePath (takeDirectory)

-- | What one typecheck produced: the error diagnostics, the top-level
-- bindings of the parsed module (none when the module did not parse), and the
-- pipelines in the typechecked module (none when it did not typecheck).
data Outcome = Outcome [MsgEnvelope GhcMessage] [Binding] [RawPipeline]

-- | The GHC-backed implementation of 'Insight'.
ghcInsight :: LibDir -> Insight IO
ghcInsight dir = Insight {analyseFile = analyse dir}

analyse :: LibDir -> SourceFile -> IO (Either InsightError Analysis)
analyse dir file = do
  failure <- preflight dir file
  maybe (guarded dir file) (pure . Left . SessionFailure) failure

-- | Check the two facts about the machine that the value types cannot carry.
preflight :: LibDir -> SourceFile -> IO (Maybe SessionError)
preflight dir file = do
  dirOk <- doesDirectoryExist (libDirPath dir)
  fileOk <- doesFileExist (sourceFilePath file)
  pure
    ( listToMaybe
        ( [LibDirNotFound (libDirPath dir) | not dirOk]
            <> [SourceFileNotFound (sourceFilePath file) | not fileOk]
        )
    )

guarded :: LibDir -> SourceFile -> IO (Either InsightError Analysis)
guarded dir file = catchSession (sourceFilePath file) (runOnce dir file)

-- | Convert the exceptions GHC and the file system can throw into a value.
catchSession :: forall a. FilePath -> IO (Either InsightError a) -> IO (Either InsightError a)
catchSession file act = act `catches` [Handler onGhc, Handler onIo]
  where
    onGhc :: GhcException -> IO (Either InsightError a)
    onGhc = failed . show
    onIo :: IOException -> IO (Either InsightError a)
    onIo = failed . show
    failed :: String -> IO (Either InsightError a)
    failed msg = pure (Left (SessionFailure (GhcThrew file (T.pack msg))))

runOnce :: LibDir -> SourceFile -> IO (Either InsightError Analysis)
runOnce dir file = do
  sink <- newIORef []
  logged <- newIORef []
  outcome <- runGhc (Just (libDirPath dir)) (typecheck sink logged path)
  captured <- readIORef sink
  evaluate (assemble path outcome captured)
  where
    path :: FilePath
    path = sourceFilePath file

typecheck :: IORef [CapturedHole] -> IORef [Text] -> FilePath -> Ghc (Either SessionError Outcome)
typecheck sink logged path = do
  configure logged
  target <- guessTarget path Nothing Nothing
  setTargets [target]
  handleSourceError (pure . Right . unparsed) (loadProject sink logged path)
  where
    -- The module could not be parsed or its imports resolved, so there are
    -- errors but no bindings.
    unparsed :: SourceError -> Outcome
    unparsed err = Outcome (errorsOf err) [] []

-- | No code generation and no linking: this session only typechecks.
--
-- GHC reports the errors of dependency modules to its log rather than by
-- throwing, so a log hook that records them, and prints nothing, is installed
-- for the whole session.
configure :: IORef [Text] -> Ghc ()
configure logged = do
  flags <- getSessionDynFlags
  setSessionDynFlags flags {backend = noBackend, ghcLink = NoLink}
  pushLogHookM (recordErrors logged)

-- | Keep the text of every error GHC logs; drop everything else.
recordErrors :: IORef [Text] -> LogAction -> LogAction
recordErrors logged _original _flags messageClass location doc
  | isErrorClass messageClass = message `seq` modifyIORef' logged (message :)
  | otherwise = pure ()
  where
    message :: Text
    message = T.pack (renderGhc (ppr location <+> doc))

isErrorClass :: MessageClass -> Bool
isErrorClass = \case
  MCFatal -> True
  MCDiagnostic SevError _ _ -> True
  _ -> False

-- | The hole-fit plugin is installed after the dependencies are loaded: only
-- holes in the analysed file are reported, and loading may reinitialise
-- plugins.
installHolePlugin :: IORef [CapturedHole] -> HscEnv -> HscEnv
installHolePlugin sink env =
  env {hsc_plugins = current {staticPlugins = staticPlugins current <> [StaticPlugin (PluginWithArgs capturing [])]}}
  where
    current :: Plugins
    current = hsc_plugins env
    capturing :: Plugin
    capturing = defaultPlugin {holeFitPlugin = holePlugin sink}

-- | Find the source folder, check the module graph, typecheck the file's
-- dependencies and then the file itself.
loadProject :: IORef [CapturedHole] -> IORef [Text] -> FilePath -> Ghc (Either SessionError Outcome)
loadProject sink logged path = do
  -- The first pass only learns the file's module name; its dependencies may
  -- not resolve yet because the source folder is still a guess.
  setImportPath (takeDirectory path)
  (_, guess) <- depanalPartial [] False
  guessed <- targetSummary path (mgModSummaries guess)
  setImportPath (maybe (takeDirectory path) (sourceRoot path . moduleNameString . ms_mod_name) guessed)
  graph <- depanal [] False
  found <- targetSummary path (mgModSummaries graph)
  case found of
    Nothing -> pure (Left (TargetNotInGraph path))
    Just summary -> do
      failure <- loadDependencies logged graph summary
      case failure of
        Just why -> pure (Left (DependencyFailed path why))
        Nothing -> do
          modifySession (installHolePlugin sink)
          Right <$> typecheckParsed sink path summary

setImportPath :: FilePath -> Ghc ()
setImportPath root = do
  flags <- getSessionDynFlags
  setSessionDynFlags flags {importPaths = [root]}

-- | The summary of the file being analysed, matched by its real path so that
-- spelling differences do not matter.
targetSummary :: FilePath -> [ModSummary] -> Ghc (Maybe ModSummary)
targetSummary path summaries = liftIO $ do
  wanted <- canonicalizePath path
  matching <- filterM (isFile wanted) summaries
  pure (listToMaybe matching)
  where
    isFile :: FilePath -> ModSummary -> IO Bool
    isFile wanted summary =
      maybe (pure False) (fmap (== wanted) . canonicalizePath) (ml_hs_file (ms_location summary))

-- | Typecheck everything the file imports from its own project. 'Just' says
-- what went wrong when a dependency did not compile.
--
-- The dependencies are loaded as the targets of their own load, rather than
-- with 'LoadDependenciesOf': in GHC 9.10.3 that also compiles the module it is
-- given, which would report the analysed file's own errors as a failed
-- dependency. The file itself is typechecked afterwards by the caller, so the
-- full module graph is put back for it.
loadDependencies :: IORef [Text] -> ModuleGraph -> ModSummary -> Ghc (Maybe Text)
loadDependencies logged graph summary
  | null names = pure Nothing
  | otherwise = do
      traverse (\name -> guessTarget name Nothing Nothing) names >>= setTargets
      outcome <- load LoadAllTargets
      modifySession (\env -> env {hsc_mod_graph = graph})
      errors <- liftIO (readIORef logged)
      pure $ case outcome of
        Succeeded -> Nothing
        Failed -> Just (describe (reverse errors))
  where
    names :: [String]
    names =
      nubOrd
        [ moduleNameString (ms_mod_name s)
        | s <- mgModSummaries graph,
          ms_mod_name s /= ms_mod_name summary
        ]
    describe :: [Text] -> Text
    describe [] = "GHC reported no details"
    describe messages = T.intercalate "; " messages

-- | Parse first, so the bindings are known even if type checking then throws.
-- When type checking fails, the pipelines are still read, from a second
-- typecheck of the same file with its errors deferred.
typecheckParsed :: IORef [CapturedHole] -> FilePath -> ModSummary -> Ghc Outcome
typecheckParsed sink path summary = do
  parsed <- parseModule summary
  let bindings = collectBindings (hsmodDecls (unLoc (pm_parsed_source parsed)))
  checked <- handleSourceError (pure . Left . errorsOf) (Right <$> typecheckModule parsed)
  case checked of
    Left errors -> Outcome errors bindings <$> deferredPipelines sink path
    Right typechecked -> pure (Outcome [] bindings (collect (tm_typechecked_source typechecked)))

-- | Typecheck the file again with type errors, typed holes and out-of-scope
-- variables deferred, so GHC produces a typechecked tree for a file that has
-- errors. Only the pipelines are kept: the diagnostics of this second pass
-- are the first pass's, and the holes it would report again are discarded.
-- Errors GHC cannot defer leave no pipelines.
--
-- In a stage GHC could not type, the type shown is what GHC inferred around
-- the error; that is where a chain's types stop lining up.
deferredPipelines :: IORef [CapturedHole] -> FilePath -> Ghc [RawPipeline]
deferredPipelines sink path = do
  seen <- liftIO (readIORef sink)
  flags <- getSessionDynFlags
  setSessionDynFlags (foldl' gopt_set flags [Opt_DeferTypeErrors, Opt_DeferTypedHoles, Opt_DeferOutOfScopeVariables])
  flows <- handleSourceError (const (pure [])) $ do
    graph <- depanal [] False
    found <- targetSummary path (mgModSummaries graph)
    case found of
      Nothing -> pure []
      Just summary -> do
        parsed <- parseModule summary
        typechecked <- typecheckModule parsed
        pure (collect (tm_typechecked_source typechecked))
  liftIO (writeIORef sink seen)
  pure flows

-- | The error diagnostics inside a 'SourceError'; warnings are not analysed.
errorsOf :: SourceError -> [MsgEnvelope GhcMessage]
errorsOf = filter isError . bagToList . getMessages . srcErrorMessages

isError :: MsgEnvelope e -> Bool
isError env = case errMsgSeverity env of
  SevError -> True
  _ -> False

assemble ::
  FilePath ->
  Either SessionError Outcome ->
  [CapturedHole] ->
  Either InsightError Analysis
assemble path outcome captured = do
  Outcome envs bindings raw <- first SessionFailure outcome
  combine
    <$> extractMessages path bindings envs
    <*> holeReports path (reverse captured)
    <*> pipelines path bindings raw
  where
    combine :: Extracted -> [HoleReport] -> [Pipeline] -> Analysis
    combine extracted holes flows =
      Analysis
        { analysisMismatches = extractedMismatches extracted,
          analysisHoles = holes,
          analysisFlows = flows,
          analysisUnexplained = extractedUnexplained extracted + unseen extracted holes
        }

-- | Typed holes GHC reported that the hole-fit plugin never saw. They are
-- counted as unexplained rather than lost. A plugin that sees a hole more than
-- once cannot make this negative.
unseen :: Extracted -> [HoleReport] -> Int
unseen extracted holes = max 0 (extractedHoleDiagnostics extracted - length holes)
