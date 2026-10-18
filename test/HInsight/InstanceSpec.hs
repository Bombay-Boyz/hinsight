module HInsight.InstanceSpec (spec) where

import Data.Text qualified as T
import HInsight.Analysis (analysisInstances, emptyAnalysis)
import HInsight.Error (DomainError (..))
import HInsight.Instance
import HInsight.Report (renderAnalysis)
import HInsight.Support (genInstanceText, genUnresolvedInstance)
import HInsight.TypeText (unTypeText)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

spec :: Spec
spec = describe "UnresolvedInstance" $ do
  describe "mkInstanceText" $ do
    it "rejects blank text" $
      mkInstanceText " \n\t" `shouldBe` Left (BlankText "instance")
    it "puts the text on one line" $
      (unInstanceText <$> mkInstanceText "instance Show\n   Int") `shouldBe` Right "instance Show Int"
  describe "shownCandidates" $ do
    prop "shows at most the limit and accounts for every instance" $
      forAll genUnresolvedInstance $ \u ->
        let (shown, omitted) = shownCandidates u
         in length shown <= shownInstances && length shown + omitted == length (unresolvedPotential u)
    prop "shows the first instances, in GHC's order" $
      forAll genUnresolvedInstance $ \u ->
        fst (shownCandidates u) == take shownInstances (unresolvedPotential u)
  describe "renderUnresolvedInstance" $ do
    prop "names the constraint" $
      forAll genUnresolvedInstance $ \u ->
        unTypeText (unresolvedConstraint u) `T.isInfixOf` renderUnresolvedInstance u
    prop "shows every instance up to the limit" $
      forAll genUnresolvedInstance $ \u ->
        all ((`T.isInfixOf` renderUnresolvedInstance u) . unInstanceText) (fst (shownCandidates u))
    prop "says when GHC lists no instance" $
      forAll genUnresolvedInstance $ \u ->
        "none listed by GHC" `T.isInfixOf` renderUnresolvedInstance u {unresolvedPotential = []}
    prop "says how many instances it leaves out" $
      forAll ((,) <$> genUnresolvedInstance <*> vectorOf (shownInstances + 2) genInstanceText) $ \(u, ts) ->
        "and 2 more" `T.isInfixOf` renderUnresolvedInstance u {unresolvedPotential = ts}
  describe "renderAnalysis" $
    prop "includes an unresolved constraint" $
      forAll genUnresolvedInstance $ \u ->
        let out = renderAnalysis emptyAnalysis {analysisInstances = [u]}
         in renderUnresolvedInstance u `T.isInfixOf` out && not ("nothing to report" `T.isInfixOf` out)
