-- |
-- Module      : HInsight.AcceptGhcSpec
-- Description : GHC judges the edits that accept a hole fit.
module HInsight.AcceptGhcSpec (spec) where

import Control.Exception (finally)
import Data.Map.Strict qualified as M
import Data.Text.IO qualified as TIO
import HInsight
import HInsight.Edit (applyEditSet, mkEditSet)
import HInsight.Environment (mkComponentName, mkEnvironment, mkGhcFlag, mkProjectRoot, pinnedGhcVersion)
import HInsight.Ghc.Session (ghcInsight, ghcInsightIn)
import HInsight.Hole (acceptFit, unModuleAlias, unRefinementLevel)
import HInsight.Support (ok)
import HInsight.Toolchain (findGhcLibDir)
import System.Directory (canonicalizePath, createDirectoryIfMissing, getTemporaryDirectory, removeDirectoryRecursive)
import System.FilePath (takeFileName, (</>))
import System.Process (getCurrentPid)
import Test.Hspec

spec :: Spec
spec = describe "acceptFit, judged by GHC" $ do
  libDir <- runIO findGhcLibDir
  maybe
    (it "accepts fits" (pendingWith "ghc-9.10.3 was not found; install it or set HINSIGHT_GHC_LIBDIR"))
    realSpec
    libDir

-- | Every fit that passes the test, accepted in its hole of the file; the
-- fit, and what GHC says of the file with that edit made.
acceptedIn :: LibDir -> Insight IO -> FilePath -> (HoleFit -> Bool) -> IO [(HoleFit, Analysis)]
acceptedIn dir insight path wanted = do
  file <- ok (mkSourceFile path)
  source <- TIO.readFile path
  original <- analyseFile insight file >>= either (fail . show) pure
  withFolder $ \folder ->
    sequence
      [ do
          edit <- ok (acceptFit file hole candidate)
          set <- ok (mkEditSet [edit])
          texts <- either (fail . show) pure (applyEditSet (const (Just source)) set)
          changed <- maybe (fail "the edit gave no text") pure (M.lookup file texts)
          let target = folder </> takeFileName path
          TIO.writeFile target changed
          targetFile <- ok (mkSourceFile target)
          checked <- analyseFile (ghcInsight dir) targetFile >>= either (fail . show) pure
          pure (candidate, checked)
      | hole <- analysisHoles original,
        candidate <- holeFits hole,
        wanted candidate
      ]

withFolder :: (FilePath -> IO a) -> IO a
withFolder action = do
  temporary <- getTemporaryDirectory
  pid <- getCurrentPid
  let folder = temporary </> ("hinsight-accept-" <> show pid)
  createDirectoryIfMissing True folder
  action folder `finally` removeDirectoryRecursive folder

realSpec :: FilePath -> Spec
realSpec libDir = do
  it "acceptedFitTypechecks: each fit with no new holes leaves no hole and no error" $ do
    dir <- ok (mkLibDir libDir)
    results <- acceptedIn dir (ghcInsight dir) "test/fixtures/accept/Accept.hs" (\f -> unRefinementLevel (fitRefinement f) == 0)
    results `shouldSatisfy` (not . null)
    mapM_ (\(_, a) -> (analysisHoles a, analysisMismatches a, analysisUnexplained a) `shouldBe` ([], [], 0)) results
  it "reads the alias of a fit imported only qualified" $ do
    dir <- ok (mkLibDir libDir)
    file <- ok (mkSourceFile "test/fixtures/accept/Accept.hs")
    a <- analyseFile (ghcInsight dir) file >>= either (fail . show) pure
    let lookups = [f | h <- analysisHoles a, f <- holeFits h, unIdentifier (fitName f) == "lookup"]
    fmap (fmap unModuleAlias . fitQualifier) lookups `shouldBe` [Just "M"]
    fmap fitIsOperator lookups `shouldBe` [False]
  it "acceptedFitWithHolesLeavesExactlyThoseHoles: a fit with n new holes leaves n holes and no mismatch" $ do
    root <- canonicalizePath "test/fixtures" >>= ok . mkProjectRoot
    component <- ok (mkComponentName "fixture")
    flags <- traverse (ok . mkGhcFlag) ["-i.", "-frefinement-level-hole-fits=1"]
    env <- ok (mkEnvironment component root pinnedGhcVersion flags)
    dir <- ok (mkLibDir libDir)
    results <- acceptedIn dir (ghcInsightIn dir env) "test/fixtures/Hole.hs" (\f -> unRefinementLevel (fitRefinement f) > 0)
    results `shouldSatisfy` (not . null)
    mapM_
      (\(f, a) -> (length (analysisHoles a), analysisMismatches a) `shouldBe` (unRefinementLevel (fitRefinement f), []))
      results
