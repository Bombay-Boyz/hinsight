-- |
-- Module      : HInsight.Ghc.Session
-- Description : Run one GHC typecheck of a file and collect what it reports.
--
-- This is the only module that starts GHC. Foreign exceptions are caught here
-- and converted into 'InsightError' immediately (Standard 1.6); nothing above
-- this module sees a GHC exception.
--
-- Limitation of this version: the file must be a single module that imports
-- only installed packages. A file that pulls in other modules of its own
-- project is reported as 'UnexpectedModuleCount'; project loading is a later
-- piece of work.
module HInsight.Ghc.Session
  ( ghcInsight,
  )
where

import Control.Exception (Handler (..), IOException, catches)
import Data.Bifunctor (first)
import Data.IORef (IORef, newIORef, readIORef)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import GHC
  ( DynFlags (..),
    Ghc,
    GhcLink (NoLink),
    depanal,
    getSessionDynFlags,
    guessTarget,
    handleSourceError,
    mgModSummaries,
    noBackend,
    parseModule,
    runGhc,
    setSessionDynFlags,
    setTargets,
    typecheckModule,
  )
import GHC.Data.Bag (bagToList)
import GHC.Driver.Env.Types (HscEnv (..))
import GHC.Driver.Errors.Types (GhcMessage)
import GHC.Driver.Monad (modifySession)
import GHC.Driver.Plugins
  ( Plugin (..),
    PluginWithArgs (..),
    Plugins (..),
    StaticPlugin (..),
    defaultPlugin,
  )
import GHC.Types.Error (MsgEnvelope (..), Severity (..), getMessages)
import GHC.Types.SourceError (SourceError, srcErrorMessages)
import GHC.Utils.Panic (GhcException)
import HInsight.Analysis (Analysis (..), Insight (..))
import HInsight.Config (LibDir, libDirPath)
import HInsight.Error (InsightError (..), SessionError (..))
import HInsight.Ghc.HoleCapture (CapturedHole, holePlugin)
import HInsight.Ghc.Holes (holeReports)
import HInsight.Ghc.Messages (Extracted (..), extractMessages)
import HInsight.Hole (HoleReport)
import HInsight.Source (SourceFile, sourceFilePath)
import System.Directory (doesDirectoryExist, doesFileExist)

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
  outcome <- runGhc (Just (libDirPath dir)) (typecheck sink path)
  captured <- readIORef sink
  pure (assemble path outcome captured)
  where
    path :: FilePath
    path = sourceFilePath file

typecheck :: IORef [CapturedHole] -> FilePath -> Ghc (Either SessionError [MsgEnvelope GhcMessage])
typecheck sink path = do
  configure sink
  target <- guessTarget path Nothing Nothing
  setTargets [target]
  handleSourceError (pure . Right . errorsOf) (typecheckOnly path)

-- | No code generation and no linking: this session only typechecks.
-- The hole-fit plugin is installed after the flags are set, because setting
-- flags can reinitialise plugins.
configure :: IORef [CapturedHole] -> Ghc ()
configure sink = do
  flags <- getSessionDynFlags
  setSessionDynFlags flags {backend = noBackend, ghcLink = NoLink}
  modifySession (installHolePlugin sink)

installHolePlugin :: IORef [CapturedHole] -> HscEnv -> HscEnv
installHolePlugin sink env =
  env {hsc_plugins = current {staticPlugins = staticPlugins current <> [StaticPlugin (PluginWithArgs capturing [])]}}
  where
    current :: Plugins
    current = hsc_plugins env
    capturing :: Plugin
    capturing = defaultPlugin {holeFitPlugin = holePlugin sink}

typecheckOnly :: FilePath -> Ghc (Either SessionError [MsgEnvelope GhcMessage])
typecheckOnly path = do
  graph <- depanal [] False
  case mgModSummaries graph of
    [summary] -> do
      _ <- parseModule summary >>= typecheckModule
      pure (Right [])
    others -> pure (Left (UnexpectedModuleCount path (length others)))

-- | The error diagnostics inside a 'SourceError'; warnings are not analysed.
errorsOf :: SourceError -> [MsgEnvelope GhcMessage]
errorsOf = filter isError . bagToList . getMessages . srcErrorMessages

isError :: MsgEnvelope e -> Bool
isError env = case errMsgSeverity env of
  SevError -> True
  _ -> False

assemble ::
  FilePath ->
  Either SessionError [MsgEnvelope GhcMessage] ->
  [CapturedHole] ->
  Either InsightError Analysis
assemble path outcome captured = do
  envs <- first SessionFailure outcome
  combine <$> extractMessages path envs <*> holeReports path (reverse captured)
  where
    combine :: Extracted -> [HoleReport] -> Analysis
    combine extracted holes =
      Analysis
        { analysisMismatches = extractedMismatches extracted,
          analysisHoles = holes,
          analysisUnexplained = extractedUnexplained extracted + unseen extracted holes
        }

-- | Typed holes GHC reported that the hole-fit plugin never saw. They are
-- counted as unexplained rather than lost. A plugin that sees a hole more than
-- once cannot make this negative.
unseen :: Extracted -> [HoleReport] -> Int
unseen extracted holes = max 0 (extractedHoleDiagnostics extracted - length holes)
