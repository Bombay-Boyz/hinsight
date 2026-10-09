module HInsight.ExplanationSpec (spec) where

import Data.Either (isRight)
import Data.Maybe (isJust)
import Data.Text qualified as T
import HInsight.Error (DomainError (..))
import HInsight.Explanation
import HInsight.Source
import HInsight.Support (genExplanation, genTypeText, ok)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

sampleExplanation :: Maybe Divergence -> IO Explanation
sampleExplanation divergence = do
  start <- mkPosition <$> ok (mkLine 3) <*> ok (mkColumn 5)
  end <- mkPosition <$> ok (mkLine 3) <*> ok (mkColumn 9)
  sp <- ok (mkSpan start end)
  expected <- ok (mkTypeText "String")
  actual <- ok (mkTypeText "Int")
  pure (Explanation sp expected actual divergence (Origin "a type signature"))

spec :: Spec
spec = do
  describe "mkTypeText" $ do
    it "rejects the empty text" $
      mkTypeText "" `shouldBe` Left (BlankText "type")
    it "rejects spaces only" $
      mkTypeText "   " `shouldBe` Left (BlankText "type")
    it "rejects tabs and newlines only" $
      mkTypeText "\t\n \r" `shouldBe` Left (BlankText "type")
    it "accepts a single character" $
      (unTypeText <$> mkTypeText "a") `shouldBe` Right "a"
    it "trims the ends" $
      (unTypeText <$> mkTypeText "  Int ") `shouldBe` Right "Int"
    it "turns a newline inside the text into one space" $
      (unTypeText <$> mkTypeText "Maybe\n   Int") `shouldBe` Right "Maybe Int"
    prop "never holds a newline" $
      forAll genTypeText $
        \t -> T.all (/= '\n') (unTypeText t)
    prop "is unchanged when rebuilt from its own text" $
      forAll genTypeText $
        \t -> mkTypeText (unTypeText t) === Right t
    prop "is accepted exactly when it has a non-space character" $ \s ->
      let t = T.pack s in isRight (mkTypeText t) === not (T.null (T.strip t))

  describe "mkDivergence" $ do
    it "rejects two identical types" $ do
      t <- ok (mkTypeText "Int")
      mkDivergence t t `shouldBe` Left (IdenticalDivergence "Int")
    it "keeps both sides in order" $ do
      l <- ok (mkTypeText "Int")
      r <- ok (mkTypeText "Bool")
      d <- ok (mkDivergence l r)
      (divergenceLeft d, divergenceRight d) `shouldBe` (l, r)

  describe "renderExplanation" $ do
    it "renders every part, without a divergence" $ do
      e <- sampleExplanation Nothing
      T.lines (renderExplanation e)
        `shouldBe` [ "type mismatch at 3:5",
                     "  expected: String",
                     "    actual: Int",
                     "  because: a type signature"
                   ]
    it "renders the divergence before the origin" $ do
      l <- ok (mkTypeText "[Char]")
      r <- ok (mkTypeText "Int")
      d <- ok (mkDivergence l r)
      e <- sampleExplanation (Just d)
      T.lines (renderExplanation e)
        `shouldBe` [ "type mismatch at 3:5",
                     "  expected: String",
                     "    actual: Int",
                     "  differs where: [Char] versus Int",
                     "  because: a type signature"
                   ]
    prop "has four lines, or five when there is a divergence" $
      forAll genExplanation $ \e ->
        length (T.lines (renderExplanation e))
          === (if isJust (explanationDivergence e) then 5 else 4)
