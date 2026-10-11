module HInsight.FlowSpec (spec) where

import Data.Text qualified as T
import HInsight.Context (Context (..))
import HInsight.Error (DomainError (..))
import HInsight.Flow
import HInsight.Source
import HInsight.Support (genPipeline, ok)
import HInsight.TypeText (mkTypeText)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

stageOf :: T.Text -> T.Text -> IO Stage
stageOf text ty = Stage <$> ok (mkStageText text) <*> ok (mkTypeText ty)

pipelineOf :: FlowKind -> [Stage] -> IO (Either DomainError Pipeline)
pipelineOf kind stages = do
  start <- mkPosition <$> ok (mkLine 6) <*> ok (mkColumn 9)
  end <- mkPosition <$> ok (mkLine 6) <*> ok (mkColumn 40)
  sp <- ok (mkSpan start end)
  whole <- ok (mkTypeText "String -> Int")
  pure (mkPipeline sp kind stages whole OutsideBinding)

spec :: Spec
spec = do
  describe "mkStageText" $ do
    it "rejects blank text" $
      mkStageText " \t\n" `shouldBe` Left (BlankText "stage")
    it "joins lines and collapses spaces" $
      fmap unStageText (mkStageText "map\n   length") `shouldBe` Right "map length"
    it "keeps text of exactly the maximum width" $ do
      let t = T.replicate 40 "a"
      fmap unStageText (mkStageText t) `shouldBe` Right t
    it "shortens longer text to the maximum width, ending in dots" $ do
      let t = T.replicate 41 "a"
      fmap unStageText (mkStageText t) `shouldBe` Right (T.replicate 37 "a" <> "...")
    prop "never exceeds 40 characters and never contains a newline" $
      forAll (listOf1 (elements "ab \n")) $ \cs ->
        either (const True) (\s -> T.length (unStageText s) <= 40 && not (T.any (== '\n') (unStageText s))) (mkStageText (T.pack cs))

  describe "mkPipeline" $ do
    it "rejects no stages" $
      pipelineOf Composition [] >>= (`shouldBe` Left (TooFewStages 0))
    it "rejects one stage" $ do
      a <- stageOf "f" "Int -> Int"
      pipelineOf Composition [a] >>= (`shouldBe` Left (TooFewStages 1))
    it "accepts two stages" $ do
      a <- stageOf "f" "Int -> Int"
      b <- stageOf "g" "Int -> Int"
      r <- pipelineOf Bind [a, b]
      fmap pipelineKind r `shouldBe` Right Bind

  describe "renderPipeline" $ do
    it "lines the stage types up and ends with the whole type" $ do
      a <- stageOf "words" "String -> [String]"
      b <- stageOf "map length" "[String] -> [Int]"
      r <- pipelineOf Composition [a, b] >>= either (fail . show) pure
      T.lines (renderPipeline r)
        `shouldBe` [ "type flow at 6:9: composition with (.)",
                     "  words      :: String -> [String]",
                     "  map length :: [String] -> [Int]",
                     "  as a whole: String -> Int"
                   ]
    prop "has a header, one line per stage and the whole type, plus context lines" $
      forAll genPipeline $ \p ->
        length (T.lines (renderPipeline p))
          === 2 + length (pipelineStages p) + contextLines (pipelineContext p)
  where
    contextLines :: Context -> Int
    contextLines = \case
      OutsideBinding -> 0
      _ -> 1
