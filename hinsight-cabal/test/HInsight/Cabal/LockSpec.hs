module HInsight.Cabal.LockSpec (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (modifyMVar_, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar)
import Control.Monad (replicateM_, unless)
import HInsight.Cabal (withProjectLock)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec = describe "withProjectLock" $
  it "projectLockIsExclusive: callers for one project never overlap, and another project is not held up" $
    withSystemTempDirectory "hinsight-lock" $ \folder -> do
      inside <- newMVar (0 :: Int)
      overlapped <- newMVar False
      done <- newEmptyMVar
      let holdOnce = withProjectLock folder "/some/project" $ do
            modifyMVar_ inside (pure . (+ 1))
            n <- readMVar inside
            unless (n == 1) (modifyMVar_ overlapped (const (pure True)))
            threadDelay 5000
            modifyMVar_ inside (pure . subtract 1)
      replicateM_ 4 (forkIO (replicateM_ 5 holdOnce >> putMVar done ()))
      replicateM_ 4 (takeMVar done)
      readMVar overlapped `shouldReturn` False
      withProjectLock folder "/some/project" (withProjectLock folder "/another/project" (pure ())) `shouldReturn` ()
