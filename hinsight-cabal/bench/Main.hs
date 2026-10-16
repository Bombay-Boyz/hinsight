-- |
-- Module      : Main
-- Description : What asking cabal for an environment costs, next to what an analysis costs.
--
-- Three timings, none thresholded (an absolute time depends on the machine,
-- and a threshold needs a stored baseline from a fixed runner, which does not
-- exist; see @docs/decisions.md@, item 6):
--
-- * capturing the environment of a file, with the build tool's cache warm;
-- * analysing the file in an environment that was captured beforehand;
-- * both together, which is what an analysis through 'ghcInsightWith' costs.
--
-- Every capture starts a @cabal@ process, so each is timed only a few times.
-- This benchmark is not part of CI's benchmark step, which runs only the core
-- package's. Run it with:
--
-- > cabal bench hinsight-cabal-bench --benchmark-options='--stdev 100'
--
-- The first capture of a project builds its dependencies into the build tool's
-- cache and is far slower than the ones timed here; it is measured by hand, as
-- described in @docs/phase-environment.md@ (step 5).
module Main (main) where

import Control.DeepSeq (NFData (..))
import Data.Text qualified as T
import HInsight (Analysis, Insight, InsightError, LibDir, analyseFile, mkLibDir, mkSourceFile, renderError)
import HInsight.Cabal (cabalEnvironment, cabalSource)
import HInsight.Environment (Environment, environmentFlags)
import HInsight.Ghc.Session (ghcInsightIn, ghcInsightWith)
import HInsight.Toolchain (findGhcLibDir)
import System.Directory (findExecutable)
import Test.Tasty.Bench

-- | The file measured. It imports the package's own modules, so analysing it
-- needs the environment's packages, not only the compiler's.
target :: FilePath
target = "src/HInsight/Cabal/Capture.hs"

-- | An environment, forced once before timing starts.
newtype Captured = Captured Environment

instance NFData Captured where
  rnf (Captured e) = rnf (length (environmentFlags e))

main :: IO ()
main = do
  libDir <- findGhcLibDir
  cabal <- findExecutable "cabal"
  case (libDir, cabal) of
    (Just dir, Just _) -> defaultMain [costs dir]
    _ -> putStrLn "skipped: needs cabal and ghc-9.10.3 (or HINSIGHT_GHC_LIBDIR)"

costs :: FilePath -> Benchmark
costs libDir =
  bgroup
    ("environment cost, " <> target)
    [ bench "capture the environment" (whnfIO capture),
      env (Captured <$> capture) $ \ ~(Captured captured) ->
        bench "analyse in a captured environment" (whnfIO (run (`ghcInsightIn` captured))),
      bench "capture and analyse" (whnfIO (run (`ghcInsightWith` cabalSource)))
    ]
  where
    capture :: IO Environment
    capture = cabalEnvironment target >>= either failed pure

    run :: (LibDir -> Insight IO) -> IO Analysis
    run make = do
      dir <- orFail (mkLibDir libDir)
      file <- orFail (mkSourceFile target)
      analyseFile (make dir) file >>= either failed pure

failed :: InsightError -> IO a
failed = fail . T.unpack . renderError

orFail :: (Show e) => Either e a -> IO a
orFail = either (fail . show) pure
