module HInsight.SuggestionSpec (spec) where

import Data.Text qualified as T
import HInsight.Analysis (analysisSuggestions, emptyAnalysis)
import HInsight.Error (DomainError (..))
import HInsight.Report (renderAnalysis)
import HInsight.Source (renderPosition, spanStart)
import HInsight.Suggestion
import HInsight.Support (genSuggested, genSuggestion)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

spec :: Spec
spec = describe "Suggestion" $ do
  describe "mkSuggestion" $ do
    it "rejects blank text" $
      mkSuggestion OtherHint " \n\t" `shouldBe` Left (BlankText "suggestion")
    it "puts the text on one line" $
      (suggestionText <$> mkSuggestion SimilarNames "Perhaps use\n   length") `shouldBe` Right "Perhaps use length"
    prop "keeps the kind and the text" $
      forAll genSuggestion $ \s ->
        mkSuggestion (suggestionKind s) (suggestionText s) == Right s
  describe "renderSuggested" $ do
    prop "names the position" $
      forAll genSuggested $ \s ->
        renderPosition (spanStart (suggestedSpan s)) `T.isInfixOf` renderSuggested s
    prop "has a heading and one line per hint" $
      forAll genSuggested $ \s ->
        length (T.lines (renderSuggested s)) == 1 + length (suggestedHints s)
    prop "shows every hint" $
      forAll genSuggested $ \s ->
        all ((`T.isInfixOf` renderSuggested s) . suggestionText) (suggestedHints s)
  describe "renderAnalysis" $
    prop "includes the suggestions" $
      forAll genSuggested $ \s ->
        renderSuggested s `T.isInfixOf` renderAnalysis emptyAnalysis {analysisSuggestions = [s]}
