{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module      : HInsight.Cabal
-- Description : Ask cabal how a file's component is compiled.
--
-- The one module of this package that starts a process. It asks @cabal@,
-- through @hie-bios@, for the arguments the file's component is compiled with
-- and for the compiler's version, and hands both to
-- "HInsight.Cabal.Capture". Each call captures afresh, so a changed @.cabal@
-- file is seen by the next call (prior-art class 20).
--
-- The build folder cabal is given is @hie-bios@'s own cache folder, never the
-- project's @dist-newstyle@ (class 11), and it is kept: the flags name package
-- databases inside it, which a later analysis must still find. @hie-bios@ locks
-- the files in it, so simultaneous calls are safe (class 21).
--
-- Limits of this version: only a cabal project is understood (a @hie.yaml@ or
-- a stack project is not consulted), and the compiler is whichever one cabal
-- selects for the project, so the project must pin GHC 9.10.3 (for example
-- @with-compiler: ghc-9.10.3@ in @cabal.project@) or the call fails with
-- 'HInsight.Error.CompilerMismatch'.
module HInsight.Cabal
  ( cabalEnvironment,
    cabalSource,
  )
where

import Colog.Core (LogAction)
import Control.Exception (IOException, try)
import Data.Text qualified as T
import Data.Void (Void, absurd)
import HIE.Bios.Config (CradleConfig (..), CradleTree (Cabal), pattern CabalType)
import HIE.Bios.Cradle (defaultCradleRunConfig, getCradleWithConfig)
import HIE.Bios.Environment (getRuntimeGhcVersion)
import HIE.Bios.Flags (getCompilerOptions)
import HIE.Bios.Types
  ( ComponentOptions (..),
    Cradle (..),
    CradleAction,
    CradleError (..),
    CradleLoadResult (..),
    LoadMode (..),
    TargetWithContext (..),
  )
import HInsight.Cabal.Capture (Captured (..), renderCaptureFailure, toEnvironment)
import HInsight.Cabal.Project (findProjectDir)
import HInsight.Environment (Environment, EnvironmentSource (..))
import HInsight.Error (EnvironmentError (..), InsightError (..))
import HInsight.Source (sourceFilePath)
import System.Directory (canonicalizePath)
import System.FilePath ((</>))

-- | The environment of the cabal component the file belongs to.
cabalEnvironment :: FilePath -> IO (Either InsightError Environment)
cabalEnvironment file = do
  found <- findProjectDir file
  case found of
    Nothing -> pure (Left (EnvironmentFailure (NoCabalProject file)))
    Just project -> do
      attempt <- try (capture project file)
      pure $ case attempt of
        Left (err :: IOException) -> Left (EnvironmentFailure (CaptureFailed (T.pack (show err))))
        Right result -> result >>= toEnvironment

-- | 'cabalEnvironment' as an 'EnvironmentSource', for
-- 'HInsight.Ghc.Session.ghcInsightWith'.
cabalSource :: EnvironmentSource IO
cabalSource = EnvironmentSource {environmentFor = cabalEnvironment . sourceFilePath}

-- | Run cabal for the file's component and read the compiler's version.
capture :: FilePath -> FilePath -> IO (Either InsightError Captured)
capture project file = do
  target <- canonicalizePath file
  cradle <- getCradleWithConfig silent defaultCradleRunConfig noCustom (cabalConfig, project)
  options <- getCompilerOptions (TargetWithContext target []) LoadFile cradle
  version <- getRuntimeGhcVersion cradle
  pure $ do
    found <- loaded file options
    reported <- loaded file version
    pure
      Captured
        { capturedRoot = cradleRootDir cradle </> componentRoot found,
          capturedOptions = componentOptions found,
          capturedVersion = reported
        }

-- | A cradle for a cabal project, with nothing taken from a @hie.yaml@.
cabalConfig :: CradleConfig Void
cabalConfig = CradleConfig [] (Cabal (CabalType Nothing Nothing Nothing))

noCustom :: Void -> CradleAction Void
noCustom = absurd

-- | Nothing the build-tool library logs is shown: a failure comes back as a
-- value, and an analysis prints nothing.
silent :: LogAction IO a
silent = mempty

loaded :: FilePath -> CradleLoadResult r -> Either InsightError r
loaded file = \case
  CradleSuccess r -> Right r
  CradleNone -> Left (EnvironmentFailure (NoCabalProject file))
  CradleFail err ->
    Left (EnvironmentFailure (CaptureFailed (renderCaptureFailure (cradleErrorExitCode err) (cradleErrorStderr err))))
