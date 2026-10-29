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
-- Two ways of placing the file are offered. 'ghcInsight' works it out from
-- the file's header and place and resolves no packages, so the imports that
-- are not the project's own must be installed. 'ghcInsightIn' takes the
-- project's 'Environment' (its flags, packages and source folders as the
-- build tool reports them), which replaces that guess.
--
-- 'ghcDeep' and 'ghcDeepIn' answer the questions that cost a second pass
-- ("HInsight.Deep"). They place the file the same way and load its
-- dependencies exactly as an analysis does, then typecheck the file once more
-- with GHC's trace switched on.
module HInsight.Ghc.Session
  ( ghcInsight,
    ghcInsightIn,
    ghcInsightWith,
    ghcDeep,
    ghcDeepIn,
    ghcDeepLimited,
  )
where

import Control.Exception (Handler (..), IOException, catches, evaluate)
import Control.Monad (filterM, void)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.Containers.ListUtils (nubOrd)
import Data.Either (fromLeft)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe, listToMaybe)
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
    compileToCoreSimplified,
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
import HInsight.Chain (ChainView, Chains (..), bindingsWithErrors, buildChains, emptyChains)
import HInsight.Config (LibDir, libDirPath)
import HInsight.Core (CoreDump (..), Optimisation, buildCore, emptyCore, isCoreMessage)
import HInsight.Deep (Deep (..))
import HInsight.Environment (Environment, EnvironmentSource (..), EnvironmentSummary, summarise)
import HInsight.Error (EnvironmentError, InsightError (..), SessionError (..))
import HInsight.Flow (Pipeline)
import HInsight.Ghc.Bindings (Binding, bindingSpans, collectBindings)
import HInsight.Ghc.Convert (renderGhc)
import HInsight.Ghc.Environment (applyEnvironment)
import HInsight.Ghc.Flow (RawPipeline, collect, pipelines)
import HInsight.Ghc.Graph (RawModule, collectGraph, projectGraph)
import HInsight.Ghc.HoleCapture (CapturedHole, holePlugin)
import HInsight.Ghc.Holes (holeReports)
import HInsight.Ghc.Messages (Extracted (..), extractMessages)
import HInsight.Ghc.Trace (captureHook, coreFlags, enableFlags, errorSpans, spliceFlags, traceFlags)
import HInsight.Graph (ProjectGraph)
import HInsight.Hole (HoleReport)
import HInsight.Project (sourceRoot)
import HInsight.Source (SourceFile, Span, sourceFilePath)
import HInsight.Splice (Splices (..), buildSplices, emptySplices, splicesIn)
import HInsight.Trace (Capture, RecordLimit, Trace (..), TraceRecord (..), defaultRecordLimit, emptyCapture, finishCapture, locatedIn)
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist)
import System.FilePath (takeDirectory)
import System.IO.Temp (withSystemTempDirectory)

-- | What one typecheck produced.
data Outcome = Outcome
  { -- | The error diagnostics.
    outcomeErrors :: [MsgEnvelope GhcMessage],
    -- | The top-level bindings of the parsed module (none when it did not
    -- parse).
    outcomeBindings :: [Binding],
    -- | The pipelines in the typechecked module (none when it did not
    -- typecheck).
    outcomePipelines :: [RawPipeline],
    -- | The project's modules and their imports (none when the file's imports
    -- did not resolve).
    outcomeModules :: [RawModule]
  }

-- | Where the analysed file's project is taken from.
data Placement
  = -- | Worked out from the file's header and place; no packages.
    FromHeader
  | -- | Given by the build tool's description of the project component.
    FromEnvironment Environment

-- | The GHC-backed implementation of 'Insight', placing the file by its
-- header.
ghcInsight :: LibDir -> Insight IO
ghcInsight dir = Insight {analyseFile = analyse dir FromHeader}

-- | The GHC-backed implementation of 'Insight' for a file of the project
-- component the 'Environment' describes.
ghcInsightIn :: LibDir -> Environment -> Insight IO
ghcInsightIn dir env = Insight {analyseFile = analyse dir (FromEnvironment env)}

-- | The GHC-backed implementation of 'Insight' that asks a source for the
-- environment of each file, then analyses the file in it. The file's existence
-- is checked first, so a missing file is reported as such and not as a failure
-- of the source.
ghcInsightWith :: LibDir -> EnvironmentSource IO -> Insight IO
ghcInsightWith dir source = Insight {analyseFile = \file -> preflight dir file >>= maybe (inEnvironment file) (pure . Left . SessionFailure)}
  where
    inEnvironment :: SourceFile -> IO (Either InsightError Analysis)
    inEnvironment file = environmentFor source file >>= either (pure . Left) (\env -> analyseFile (ghcInsightIn dir env) file)

analyse :: LibDir -> Placement -> SourceFile -> IO (Either InsightError Analysis)
analyse dir placement file = do
  failure <- preflight dir file
  maybe (guarded dir placement file) (pure . Left . SessionFailure) failure

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

guarded :: LibDir -> Placement -> SourceFile -> IO (Either InsightError Analysis)
guarded dir placement file = catchSession (sourceFilePath file) (runOnce dir placement file)

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

runOnce :: LibDir -> Placement -> SourceFile -> IO (Either InsightError Analysis)
runOnce dir placement file = do
  sink <- newIORef []
  logged <- newIORef []
  outcome <- runGhc (Just (libDirPath dir)) (typecheck placement sink logged path)
  captured <- readIORef sink
  evaluate (assemble path (summaryOf placement) outcome captured)
  where
    path :: FilePath
    path = sourceFilePath file

typecheck :: Placement -> IORef [CapturedHole] -> IORef [Text] -> FilePath -> Ghc (Either InsightError Outcome)
typecheck placement sink logged path = do
  configured <- configure placement logged
  case configured of
    Left failure -> pure (Left (EnvironmentFailure failure))
    Right () -> do
      -- GHC resolves a relative target against the environment's working
      -- directory, not the process's, so the target is given as an absolute
      -- path; the path as the caller wrote it stays the label of the results.
      located <- liftIO (targetPath placement path)
      target <- guessTarget located Nothing Nothing
      setTargets [target]
      first SessionFailure <$> handleSourceError (pure . Right . unparsed) (loadProject placement sink logged path)
  where
    -- The module could not be parsed or its imports resolved, so there are
    -- errors but no bindings.
    unparsed :: SourceError -> Outcome
    unparsed err = Outcome (errorsOf err) [] [] []

-- | The path the target is given to GHC by.
targetPath :: Placement -> FilePath -> IO FilePath
targetPath FromHeader path = pure path
targetPath (FromEnvironment _) path = canonicalizePath path

-- | Apply the environment's flags, if there is one, then switch off code
-- generation and linking: this session only typechecks. The two overrides come
-- last so that no flag of the project can turn them back on.
--
-- GHC reports the errors of dependency modules to its log rather than by
-- throwing, so a log hook that records them, and prints nothing, is installed
-- for the whole session.
configure :: Placement -> IORef [Text] -> Ghc (Either EnvironmentError ())
configure placement logged = do
  pushLogHookM (recordErrors logged)
  initial <- getSessionDynFlags
  applied <- case placement of
    FromHeader -> pure (Right initial)
    FromEnvironment env -> applyEnvironment env initial
  traverse (\flags -> void (setSessionDynFlags flags {backend = noBackend, ghcLink = NoLink})) applied

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

-- | Find the source folder, check the module graph and typecheck the file's
-- dependencies. The module graph and the file's own summary come back for
-- whoever typechecks the file itself.
prepareProject :: Placement -> IORef [Text] -> FilePath -> Ghc (Either SessionError (ModuleGraph, ModSummary))
prepareProject placement logged path = do
  case placement of
    FromEnvironment _ -> pure ()
    FromHeader -> do
      -- The first pass only learns the file's module name; its dependencies
      -- may not resolve yet because the source folder is still a guess.
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
      pure (maybe (Right (graph, summary)) (Left . DependencyFailed path) failure)

-- | Prepare the project, then typecheck the file itself.
loadProject :: Placement -> IORef [CapturedHole] -> IORef [Text] -> FilePath -> Ghc (Either SessionError Outcome)
loadProject placement sink logged path = do
  prepared <- prepareProject placement logged path
  case prepared of
    Left failure -> pure (Left failure)
    Right (graph, summary) -> do
      modifySession (installHolePlugin sink)
      Right . (\outcome -> outcome {outcomeModules = collectGraph graph}) <$> typecheckParsed sink path summary

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
        Failed -> Just (describeErrors (reverse errors))
  where
    names :: [String]
    names =
      nubOrd
        [ moduleNameString (ms_mod_name s)
        | s <- mgModSummaries graph,
          ms_mod_name s /= ms_mod_name summary
        ]

-- | What GHC reported, on one line.
describeErrors :: [Text] -> Text
describeErrors [] = "GHC reported no details"
describeErrors messages = T.intercalate "; " messages

-- | Parse first, so the bindings are known even if type checking then throws.
-- When type checking fails, the pipelines are still read, from a second
-- typecheck of the same file with its errors deferred.
typecheckParsed :: IORef [CapturedHole] -> FilePath -> ModSummary -> Ghc Outcome
typecheckParsed sink path summary = do
  parsed <- parseModule summary
  let bindings = collectBindings (hsmodDecls (unLoc (pm_parsed_source parsed)))
  checked <- handleSourceError (pure . Left . errorsOf) (Right <$> typecheckModule parsed)
  case checked of
    Left errors -> (\flows -> Outcome errors bindings flows []) <$> deferredPipelines sink path
    Right typechecked -> pure (Outcome [] bindings (collect (tm_typechecked_source typechecked)) [])

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

-- | The GHC-backed implementation of 'Deep', placing the file by its header.
ghcDeep :: LibDir -> Deep IO
ghcDeep dir = deepIn dir FromHeader defaultRecordLimit

-- | 'ghcDeep' with a record limit of the caller's choosing.
ghcDeepLimited :: RecordLimit -> LibDir -> Deep IO
ghcDeepLimited limit dir = deepIn dir FromHeader limit

-- | The GHC-backed implementation of 'Deep' for a file of the project
-- component the 'Environment' describes.
ghcDeepIn :: LibDir -> Environment -> Deep IO
ghcDeepIn dir env = deepIn dir (FromEnvironment env) defaultRecordLimit

deepIn :: LibDir -> Placement -> RecordLimit -> Deep IO
deepIn dir placement limit =
  Deep
    { chainsFor = \view file ->
        deep dir placement limit ErroringBindings traceFlags locatedIn (chainsOf view (sourceFilePath file)) file,
      splicesFor =
        deep dir placement limit EveryBinding spliceFlags (\ghcPath _ -> splicesIn ghcPath) splicesOf,
      coreOf = core dir placement
    }

-- | Make the Core of a file and read what GHC logged. Anything GHC writes goes
-- to a folder made for this call and removed when it ends, however it ends.
core :: LibDir -> Placement -> Optimisation -> SourceFile -> IO (Either InsightError CoreDump)
core dir placement optimisation file = do
  failure <- preflight dir file
  case failure of
    Just why -> pure (Left (SessionFailure why))
    Nothing -> catchSession path $ withSystemTempDirectory "hinsight-core" $ \folder -> do
      logged <- newIORef []
      result <- runGhc (Just (libDirPath dir)) (corePass placement optimisation folder logged path)
      evaluate result
  where
    path :: FilePath
    path = sourceFilePath file

-- | Load the project, typecheck the file, then ask GHC for its simplified Core
-- with the Core dumps on. Typechecking first tells a file that does not
-- typecheck (GHC is blocked, 'coreBlocked') from a Core step that failed on a
-- file that does ('CoreFailed'). Only the file is desugared and simplified, so
-- the rule firings are its own; its dependencies are only typechecked.
corePass :: Placement -> Optimisation -> FilePath -> IORef [Text] -> FilePath -> Ghc (Either InsightError CoreDump)
corePass placement optimisation folder logged path = do
  prepared <- prepareDeep placement logged path
  case prepared of
    Left failure -> pure (Left failure)
    Right (Left blocked) -> pure (Right (blocked' blocked))
    Right (Right located) -> do
      enabled <- enableFlags (coreFlags optimisation folder)
      case enabled of
        Left failure -> pure (Left (EnvironmentFailure failure))
        Right () -> do
          guessTarget located Nothing Nothing >>= setTargets . pure
          graph <- depanal [] False
          found <- targetSummary path (mgModSummaries graph)
          case found of
            Nothing -> pure (Left (SessionFailure (TargetNotInGraph path)))
            Just summary -> do
              checked <- handleSourceError (pure . Left . errorsOf) (Right <$> (parseModule summary >>= typecheckModule))
              case checked of
                Left errors -> pure (Right (blocked' (length errors)))
                Right _ -> makeCore located
  where
    blocked' :: Int -> CoreDump
    blocked' n = (emptyCore optimisation) {coreBlocked = n}
    makeCore :: FilePath -> Ghc (Either InsightError CoreDump)
    makeCore located = do
      ref <- liftIO (newIORef (emptyCapture defaultRecordLimit))
      pushLogHookM (captureHook ref isCoreMessage)
      -- GHC adds the file as a target itself, and a file given twice is an
      -- error, so the targets are cleared first.
      setTargets []
      made <- handleSourceError (pure . Left . errorsOf) (Right <$> compileToCoreSimplified located)
      captured <- liftIO (readIORef ref)
      errors <- liftIO (readIORef logged)
      pure $ do
        trace <- first TraceFailure (finishCapture captured)
        case made of
          Left _ -> Left (SessionFailure (CoreFailed path (describeErrors (reverse errors))))
          Right _ -> Right (buildCore optimisation (map recordRaw (traceRecords trace)))

-- | Run a second pass over a file and read what it logged.
deep ::
  LibDir ->
  Placement ->
  RecordLimit ->
  Scoping ->
  [String] ->
  (FilePath -> [Span] -> TraceRecord -> Bool) ->
  (Deepened -> Either InsightError a) ->
  SourceFile ->
  IO (Either InsightError a)
deep dir placement limit scoping flags wanted finish file = do
  failure <- preflight dir file
  case failure of
    Just why -> pure (Left (SessionFailure why))
    Nothing -> catchSession path $ do
      logged <- newIORef []
      result <- runGhc (Just (libDirPath dir)) (deepPass placement limit scoping flags wanted logged path)
      evaluate (result >>= finish)
  where
    path :: FilePath
    path = sourceFilePath file

-- | What preparing a project for a second pass came to.
data Prepared
  = -- | The dependencies are loaded and the file is in the module graph.
    Ready
  | -- | The project could not be prepared.
    Refused SessionError
  | -- | GHC stopped on errors before the file could be typechecked, for
    -- example a parse error in its header or an import it cannot find.
    Unreadable [MsgEnvelope GhcMessage]

-- | What a second pass reached.
data Deepened
  = -- | GHC stopped before it typechecked the file, on this many errors.
    Blocked Int
  | -- | GHC typechecked the file.
    Ran DeepRun

-- | A typecheck of the file with a dump on.
data DeepRun = DeepRun
  { -- | The messages the dump logged that the caller wanted.
    runCaptured :: Capture,
    -- | The spans of the file's top-level bindings.
    runBindings :: [Span],
    -- | The errors of the typecheck; none when it passed.
    runErrors :: [MsgEnvelope GhcMessage]
  }

-- | Configure the session and load the project as an analysis does. 'Left' says
-- how many errors stopped GHC before the file could be typechecked; 'Right' is
-- the target as GHC was given it.
prepareDeep :: Placement -> IORef [Text] -> FilePath -> Ghc (Either InsightError (Either Int FilePath))
prepareDeep placement logged path = do
  configured <- configure placement logged
  case configured of
    Left failure -> pure (Left (EnvironmentFailure failure))
    Right () -> do
      located <- liftIO (targetPath placement path)
      target <- guessTarget located Nothing Nothing
      setTargets [target]
      prepared <- handleSourceError (pure . Unreadable . errorsOf) (either Refused (const Ready) <$> prepareProject placement logged path)
      pure $ case prepared of
        Refused why -> Left (SessionFailure why)
        Unreadable errors -> Right (Left (length errors))
        Ready -> Right (Right located)

-- | Load the project as an analysis does, then typecheck the file once more
-- with these flags on, giving the messages they log to the 'Capture'.
deepPass ::
  Placement ->
  RecordLimit ->
  Scoping ->
  [String] ->
  (FilePath -> [Span] -> TraceRecord -> Bool) ->
  IORef [Text] ->
  FilePath ->
  Ghc (Either InsightError Deepened)
deepPass placement limit scoping flags wanted logged path = do
  prepared <- prepareDeep placement logged path
  case prepared of
    Left failure -> pure (Left failure)
    Right (Left blocked) -> pure (Right (Blocked blocked))
    Right (Right located) -> passWithFlags limit scoping flags wanted located path

-- | The second pass. The module graph is built again after the flags are set,
-- because GHC typechecks a module with the flags in its summary. Loading the
-- file's dependencies left them as the targets, so the file is made the target
-- again first; without it a file that imports another module of its project
-- is not in the graph.
passWithFlags :: RecordLimit -> Scoping -> [String] -> (FilePath -> [Span] -> TraceRecord -> Bool) -> FilePath -> FilePath -> Ghc (Either InsightError Deepened)
passWithFlags limit scoping flags wanted located path = do
  scoped <- scopeOf scoping located path
  case scoped of
    Left failure -> pure (Left failure)
    Right Nothing -> pure (Right (Ran (DeepRun (emptyCapture limit) [] [])))
    Right (Just keep) -> do
      enabled <- enableFlags flags
      case enabled of
        Left failure -> pure (Left (EnvironmentFailure failure))
        Right () -> do
          guessTarget located Nothing Nothing >>= setTargets . pure
          graph <- depanal [] False
          found <- targetSummary path (mgModSummaries graph)
          case found of
            Nothing -> pure (Left (SessionFailure (TargetNotInGraph path)))
            Just summary -> do
              parsed <- handleSourceError (pure . Left . errorsOf) (Right <$> parseModule summary)
              case parsed of
                Left errors -> pure (Right (Blocked (length errors)))
                Right module' -> typecheckCaptured limit keep wanted path summary module'

-- | Which bindings of the file the trace keeps records of.
data Scoping
  = EveryBinding
  | ErroringBindings

-- | The bindings to keep records of: 'Nothing' when the file has no errors, so
-- the pass has nothing to explain.
scopeOf :: Scoping -> FilePath -> FilePath -> Ghc (Either InsightError (Maybe ([Span] -> [Span])))
scopeOf EveryBinding _ _ = pure (Right (Just id))
scopeOf ErroringBindings located path = do
  guessTarget located Nothing Nothing >>= setTargets . pure
  graph <- depanal [] False
  found <- targetSummary path (mgModSummaries graph)
  case found of
    Nothing -> pure (Left (SessionFailure (TargetNotInGraph path)))
    Just summary -> do
      checked <- handleSourceError (pure . Left . errorsOf) (Right <$> (parseModule summary >>= typecheckModule))
      pure $ case checked of
        Right _ -> Right Nothing
        Left errors -> Just . bindingsWithErrors <$> errorSpans path errors

typecheckCaptured :: RecordLimit -> ([Span] -> [Span]) -> (FilePath -> [Span] -> TraceRecord -> Bool) -> FilePath -> ModSummary -> ParsedModule -> Ghc (Either InsightError Deepened)
typecheckCaptured limit keep wanted path summary parsed =
  case bindingSpans path (collectBindings (hsmodDecls (unLoc (pm_parsed_source parsed)))) of
    Left failure -> pure (Left failure)
    Right spans -> do
      ref <- liftIO (newIORef (emptyCapture limit))
      -- The dump names the file as GHC knows it.
      let ghcPath = fromMaybe path (ml_hs_file (ms_location summary))
      pushLogHookM (captureHook ref (wanted ghcPath (keep spans)))
      checked <- handleSourceError (pure . Left . errorsOf) (Right <$> typecheckModule parsed)
      captured <- liftIO (readIORef ref)
      pure (Right (Ran (DeepRun captured spans (fromLeft [] checked))))

-- | The chains for the errors of a typecheck, from what the trace collected.
-- Past the record limit the answer is an error, not a part of the trace. A
-- file with no errors has no chains whatever the trace holds.
chainsOf :: ChainView -> FilePath -> Deepened -> Either InsightError Chains
chainsOf _ _ (Blocked n) = Right (Chains [] n)
chainsOf view path (Ran run)
  | null errors = Right emptyChains
  | otherwise = do
      trace <- first TraceFailure (finishCapture (runCaptured run))
      spans <- errorSpans path errors
      let built = buildChains view (runBindings run) spans (traceRecords trace)
      -- An error with no real span cannot have a chain.
      pure built {chainsMissing = chainsMissing built + length errors - length spans}
  where
    errors :: [MsgEnvelope GhcMessage]
    errors = runErrors run

-- | The splices GHC ran, from what the dump logged. Splices dumped before an
-- error are reported with it; past the record limit the answer is an error.
splicesOf :: Deepened -> Either InsightError Splices
splicesOf (Blocked n) = Right emptySplices {splicesBlocked = n}
splicesOf (Ran run) = do
  trace <- first TraceFailure (finishCapture (runCaptured run))
  pure (buildSplices (map recordRaw (traceRecords trace)))

-- | The error diagnostics inside a 'SourceError'; warnings are not analysed.
errorsOf :: SourceError -> [MsgEnvelope GhcMessage]
errorsOf = filter isError . bagToList . getMessages . srcErrorMessages

isError :: MsgEnvelope e -> Bool
isError env = case errMsgSeverity env of
  SevError -> True
  _ -> False

-- | What the analysis says about the environment it ran in.
summaryOf :: Placement -> Maybe EnvironmentSummary
summaryOf FromHeader = Nothing
summaryOf (FromEnvironment env) = Just (summarise env)

assemble ::
  FilePath ->
  Maybe EnvironmentSummary ->
  Either InsightError Outcome ->
  [CapturedHole] ->
  Either InsightError Analysis
assemble path environment outcome captured = do
  found <- outcome
  combine
    <$> extractMessages path (outcomeBindings found) (outcomeErrors found)
    <*> holeReports path (reverse captured)
    <*> pipelines path (outcomeBindings found) (outcomePipelines found)
    <*> projectGraph path (outcomeModules found)
  where
    combine :: Extracted -> [HoleReport] -> [Pipeline] -> ProjectGraph -> Analysis
    combine extracted holes flows graph =
      Analysis
        { analysisMismatches = extractedMismatches extracted,
          analysisInstances = extractedInstances extracted,
          analysisSuggestions = extractedSuggestions extracted,
          analysisHoles = holes,
          analysisFlows = flows,
          analysisGraph = graph,
          analysisUnexplained = extractedUnexplained extracted + unseen extracted holes,
          analysisEnvironment = environment
        }

-- | Typed holes GHC reported that the hole-fit plugin never saw. They are
-- counted as unexplained rather than lost. A plugin that sees a hole more than
-- once cannot make this negative.
unseen :: Extracted -> [HoleReport] -> Int
unseen extracted holes = max 0 (extractedHoleDiagnostics extracted - length holes)
