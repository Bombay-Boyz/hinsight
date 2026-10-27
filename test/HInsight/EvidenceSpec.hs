-- |
-- Module      : HInsight.EvidenceSpec
-- Description : Every error GHC reports is accounted for, and the rigid-variable case is explained.
--
-- Runs a real GHC, like "HInsight.GhcSessionSpec", and is reported as pending
-- when none is found. The accounting check is the invariant of the explanation
-- phase: explained mismatches, unresolved constraints, typed holes and
-- unexplained errors add up to the number of errors GHC reported.
module HInsight.EvidenceSpec (spec) where

import Data.Foldable (for_)
import Data.Text qualified as T
import HInsight
import HInsight.Ghc.Session (ghcInsight)
import HInsight.Support (ok)
import HInsight.Toolchain (findGhcLibDir)
import HInsight.TypeText (unTypeText)
import Test.Hspec

analyse :: FilePath -> FilePath -> IO Analysis
analyse libDir name = do
  dir <- ok (mkLibDir libDir)
  file <- ok (mkSourceFile ("test/fixtures/" <> name))
  result <- analyseFile (ghcInsight dir) file
  either (fail . T.unpack . renderError) pure result

-- | Every finding of an analysis that stands for one GHC error.
accountedFor :: Analysis -> Int
accountedFor a =
  length (analysisMismatches a)
    + length (analysisInstances a)
    + length (analysisHoles a)
    + analysisUnexplained a

spec :: Spec
spec = describe "evidence for GHC's errors" $ do
  libDir <- runIO findGhcLibDir
  maybe
    (it "accounts for errors" (pendingWith "ghc-9.10.3 was not found; install it or set HINSIGHT_GHC_LIBDIR"))
    evidenceSpec
    libDir

evidenceSpec :: FilePath -> Spec
evidenceSpec libDir = do
  it "explains a mismatch against a rigid type variable" $ do
    a <- analyse libDir "RigidVariable.hs"
    case analysisMismatches a of
      [m] -> do
        unTypeText (explanationExpected m) `shouldBe` "Int"
        unTypeText (explanationActual m) `shouldBe` "a"
        case explanationContext m of
          InSignedBinding name _ _ -> unIdentifier name `shouldBe` "h"
          other -> expectationFailure ("expected a signed binding, found " <> show other)
      other -> expectationFailure ("expected exactly one mismatch, found " <> show (length other))
    analysisUnexplained a `shouldBe` 0
  it "still explains a plain mismatch" $ do
    a <- analyse libDir "TypeMismatch.hs"
    length (analysisMismatches a) `shouldBe` 1
  it "explains every mismatch in a file that has several" $ do
    a <- analyse libDir "ThreeMismatches.hs"
    (length (analysisMismatches a), analysisUnexplained a) `shouldBe` (3, 0)
  it "reports a missing instance with the constraint GHC could not solve" $ do
    a <- analyse libDir "NoInstance.hs"
    case analysisInstances a of
      [u] -> do
        unTypeText (unresolvedConstraint u) `shouldBe` "Show T"
        unresolvedPotential u `shouldBe` []
      other -> expectationFailure ("expected exactly one unresolved constraint, found " <> show (length other))
    (length (analysisMismatches a), analysisUnexplained a) `shouldBe` (0, 0)
    analysisSuggestions a `shouldBe` []
  it "lists the instances GHC says could match an ambiguous constraint" $ do
    a <- analyse libDir "AmbiguousLength.hs"
    let constraints = map (unTypeText . unresolvedConstraint) (analysisInstances a)
    length constraints `shouldBe` 2
    filter ("Foldable" `T.isPrefixOf`) constraints `shouldSatisfy` (not . null)
    concatMap unresolvedPotential (analysisInstances a) `shouldSatisfy` (not . null)
    analysisUnexplained a `shouldBe` 0
  it "carries the hint GHC gives for a misspelt name" $ do
    a <- analyse libDir "SimilarName.hs"
    analysisUnexplained a `shouldBe` 1
    case analysisSuggestions a of
      [s] -> do
        SimilarNames `elem` fmap suggestionKind (suggestedHints s) `shouldBe` True
        any (("length" `T.isInfixOf`) . suggestionText) (suggestedHints s) `shouldBe` True
      other -> expectationFailure ("expected one suggestion, found " <> show (length other))
  it "reports no mismatch next to a scope error: GHC stops before it typechecks" $ do
    a <- analyse libDir "ScopeHidesMismatch.hs"
    (length (analysisMismatches a), analysisUnexplained a) `shouldBe` (0, 1)
  it "reports a mismatch next to a typed hole, because GHC does" $ do
    a <- analyse libDir "HoleAndMismatch.hs"
    (length (analysisMismatches a), length (analysisHoles a), analysisUnexplained a) `shouldBe` (1, 1, 0)
  it "prints the type of a hole and of its fits without type variable uniques" $ do
    a <- analyse libDir "HoleVariable.hs"
    case analysisHoles a of
      [h] -> do
        unTypeText (holeType h) `shouldBe` "[a] -> [a]"
        let printed = unTypeText (holeType h) : map (unTypeText . fitType) (holeFits h)
        filter (T.isInfixOf "[sk:") printed `shouldBe` []
        filter (T.isInfixOf "_a") printed `shouldBe` []
      other -> expectationFailure ("expected exactly one hole, found " <> show (length other))
  it "accounts for every error GHC reports" $
    for_ [("TypeMismatch.hs", 1), ("RigidVariable.hs", 1), ("ScopeError.hs", 1), ("Hole.hs", 1), ("ThreeMismatches.hs", 3), ("NoInstance.hs", 1), ("AmbiguousLength.hs", 2), ("SimilarName.hs", 1)] $
      \(name, reported) -> do
        a <- analyse libDir name
        (name, accountedFor a) `shouldBe` (name, reported)
