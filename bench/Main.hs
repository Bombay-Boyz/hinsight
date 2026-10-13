-- |
-- Module      : Main
-- Description : Benchmarks for the code whose cost depends on input size, and for one analysis.
--
-- The ranking benchmark is compared with the plain sort it wraps, so its
-- threshold holds on any machine: CI fails when ranking is more than a set
-- percentage slower than the sort (see the workflow). The analysis benchmarks
-- need a GHC 9.10.3 and are reported, not thresholded, because their absolute
-- times depend on the machine.
module Main (main) where

import Control.DeepSeq (NFData (..))
import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import HInsight
import HInsight.Ghc.Session (ghcInsight)
import HInsight.Hole (Locality (..), mkRefinementLevel)
import HInsight.Report (renderAnalysis)
import HInsight.Source (mkColumn, mkLine, mkPosition, mkSpan)
import HInsight.Toolchain (findGhcLibDir)
import HInsight.TypeText (mkTypeText)
import Test.Tasty.Bench

main :: IO ()
main = do
  libDir <- findGhcLibDir
  defaultMain
    ( [rankingGroup, renderingGroup]
        <> maybe [] (pure . analysisGroup) libDir
    )

-- | Fits ready to rank, forced once before timing starts.
newtype Fits = Fits [HoleFit]

instance NFData Fits where
  rnf (Fits fits) = foldr seq () fits

-- | An analysis ready to render, forced once before timing starts by
-- rendering it.
newtype Built = Built Analysis

instance NFData Built where
  rnf (Built a) = rnf (renderAnalysis a)

rankingGroup :: Benchmark
rankingGroup = bgroup "rank fits" (map sized [1_000, 10_000, 100_000])
  where
    sized :: Int -> Benchmark
    sized n =
      env (pure (Fits (mapMaybe syntheticFit [1 .. n]))) $ \ ~(Fits fits) ->
        bgroup
          (show n)
          [ bench "plain sort" (whnf (length . sortOn fitName) fits),
            bcompare ("$(NF-1) == \"" <> show n <> "\" && $NF == \"plain sort\"") $
              bench "rankFits" (whnf (length . rankFits) fits)
          ]

renderingGroup :: Benchmark
renderingGroup = bgroup "render analysis" (map sized [100, 1_000, 10_000])
  where
    sized :: Int -> Benchmark
    sized n =
      env (pure (Built (analysisWithHoles n))) $ \ ~(Built a) ->
        bench (show n <> " holes") (nf renderAnalysis a)

-- | Analyse real files. One analysis starts a GHC session, so these take
-- much longer than the pure benchmarks and are timed only a few times.
analysisGroup :: FilePath -> Benchmark
analysisGroup libDir =
  bgroup
    "analyse a file"
    [ analyse "a single module" "test/fixtures/Ok.hs",
      analyse "a module and two project imports" "test/fixtures/project/App.hs"
    ]
  where
    analyse :: String -> FilePath -> Benchmark
    analyse name path = bench name (whnfIO (run path))
    run :: FilePath -> IO (Either InsightError Analysis)
    run path = do
      dir <- orFail (mkLibDir libDir)
      file <- orFail (mkSourceFile path)
      analyseFile (ghcInsight dir) file

orFail :: (Show e) => Either e a -> IO a
orFail = either (fail . show) pure

hush :: Either e a -> Maybe a
hush = either (const Nothing) Just

-- | A fit whose name, type and level repeat, so the sort has ties to break.
syntheticFit :: Int -> Maybe HoleFit
syntheticFit i = do
  name <- hush (mkIdentifier (T.pack ("f" <> show (i `mod` 977))))
  ty <- hush (mkTypeText (T.pack ("Int -> T" <> show (i `mod` 13))))
  level <- hush (mkRefinementLevel (i `mod` 3))
  pure (HoleFit name ty level (if even i then Local else Imported) [])

analysisWithHoles :: Int -> Analysis
analysisWithHoles n = emptyAnalysis {analysisHoles = mapMaybe holeAt [1 .. n]}

holeAt :: Int -> Maybe HoleReport
holeAt i = do
  line <- hush (mkLine i)
  start <- mkPosition line <$> hush (mkColumn 1)
  end <- mkPosition line <$> hush (mkColumn 2)
  sp <- hush (mkSpan start end)
  name <- hush (mkIdentifier "_")
  ty <- hush (mkTypeText "Int")
  pure (HoleReport name sp ty (mapMaybe syntheticFit [1 .. 5]))
