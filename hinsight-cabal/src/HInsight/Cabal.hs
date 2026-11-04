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
-- its own files but not the build folder cabal configures, and two first-time
-- calls for one project configure it at once and one fails. So the calls for
-- one project take turns, through a lock file next to @hie-bios@'s cache
-- (class 21). The lock is held by the operating system, so it also serialises
-- separate processes, and it is released if the process dies.
--
-- Limits of this version: only a cabal project is understood (a @hie.yaml@ or
-- a stack project is not consulted), and the compiler is whichever one cabal
-- selects for the project, so the project must pin GHC 9.10.3 (for example
-- @with-compiler: ghc-9.10.3@ in @cabal.project@) or the call fails with
-- 'HInsight.Error.CompilerMismatch'.
module HInsight.Cabal
  ( cabalEnvironment,
    cabalSource,
    withProjectLock,
  )
where

import Colog.Core (LogAction)
import Control.Concurrent (threadDelay)
import Control.Exception (IOException, bracket, throwIO, try)
import Data.Bits (xor)
import Data.Char (ord)
import Data.Text qualified as T
import Data.Void (Void, absurd)
import Data.Word (Word64)
import GHC.IO.Handle.Lock (LockMode (ExclusiveLock), hTryLock)
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
import Numeric (showHex)
import System.Directory (XdgDirectory (XdgCache), canonicalizePath, createDirectoryIfMissing, getXdgDirectory)
import System.FilePath ((</>))
import System.IO (Handle, IOMode (ReadWriteMode), hClose, openFile)
import System.IO.Error (isAlreadyInUseError)

-- | The environment of the cabal component the file belongs to.
cabalEnvironment :: FilePath -> IO (Either InsightError Environment)
cabalEnvironment file = do
  found <- findProjectDir file
  case found of
    Nothing -> pure (Left (EnvironmentFailure (NoCabalProject file)))
    Just project -> do
      cache <- getXdgDirectory XdgCache "hie-bios"
      attempt <- try (withProjectLock cache project (capture project file))
      pure $ case attempt of
        Left (err :: IOException) -> Left (EnvironmentFailure (CaptureFailed (T.pack (show err))))
        Right result -> result >>= toEnvironment

-- | 'cabalEnvironment' as an 'EnvironmentSource', for
-- 'HInsight.Ghc.Session.ghcInsightWith'.
cabalSource :: EnvironmentSource IO
cabalSource = EnvironmentSource {environmentFor = cabalEnvironment . sourceFilePath}

-- | Run an action while holding the lock for a project. The lock is a file in
-- the given folder, named after the project, so the folder is created if it is
-- missing. Callers wait, polling, rather than block, so this is safe on the
-- non-threaded runtime too. The lock is released when the action ends, however
-- it ends.
withProjectLock :: FilePath -> FilePath -> IO a -> IO a
withProjectLock folder project action = do
  createDirectoryIfMissing True folder
  bracket (openLockFile (folder </> ("hinsight-capture-" <> fingerprint project <> ".lock"))) hClose $ \h -> do
    let acquire =
          hTryLock h ExclusiveLock >>= \case
            True -> pure ()
            False -> threadDelay pollMicros >> acquire
    acquire
    action

-- | Open the lock file for writing. While another thread of this process holds
-- it the open fails with "already in use", so wait and try again.
openLockFile :: FilePath -> IO Handle
openLockFile path =
  try (openFile path ReadWriteMode) >>= \case
    Right h -> pure h
    Left err
      | isAlreadyInUseError err -> threadDelay pollMicros >> openLockFile path
      | otherwise -> throwIO err

-- | How long to wait between attempts to take a lock.
pollMicros :: Int
pollMicros = 20000

-- | A short name for a path, fit for a file name: the FNV-1a hash in hex. Two
-- projects that share one only wait for each other.
fingerprint :: FilePath -> String
fingerprint p = showHex (foldl' (\h c -> (h `xor` fromIntegral (ord c)) * 1099511628211) (14695981039346656037 :: Word64) p) ""

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
