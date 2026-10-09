module HInsight.ExplanationSpec (spec) where

import Data.Either (isRight)
import Data.Maybe (isJust)
import Data.Text qualified as T
import HInsight.Error (DomainError (..))
import HInsight.Explanation
import HInsight.Identifier (mkIdentifier)
import HInsight.Source
import HInsight.Support (genExplanation, genTypeText, ok)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

sampleExplanation :: Maybe Divergence -> Context -> IO Explanation
sampleExplanation divergence ctx = do
  start <- mkPosition <$> ok (mkLine 4) <*> ok (mkColumn 12)
  end <- mkPosition <$> ok (mkLine 4) <*> ok (mkColumn 25)
  sp <- ok (mkSpan start end)
  expected <- ok (mkTypeText "String")
  actual <- ok (mkTypeText "Int")
  pure (Explanation sp expected actual divergence ctx)

signedContext :: IO Context
signedContext = do
  name <- ok (mkIdentifier "greeting")
  start <- mkPosition <$> ok (mkLine 3) <*> ok (mkColumn 1)
  sp <- ok (mkSpan start start)
  declared <- ok (mkTypeText "String")
  pure (InSignedBinding name sp declared)

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
    it "renders only the types when there is no divergence or context" $ do
      e <- sampleExplanation Nothing OutsideBinding
      T.lines (renderExplanation e)
        `shouldBe` [ "type mismatch at 4:12",
                     "  expected: String",
                     "    actual: Int"
                   ]
    it "says which two types could not be matched, in GHC's order" $ do
      l <- ok (mkTypeText "Int")
      r <- ok (mkTypeText "[Char]")
      d <- ok (mkDivergence l r)
      e <- sampleExplanation (Just d) OutsideBinding
      T.lines (renderExplanation e)
        `shouldContain` ["  could not match: Int with [Char]"]
    it "names the binding and its declared type" $ do
      c <- signedContext
      e <- sampleExplanation Nothing c
      T.lines (renderExplanation e)
        `shouldContain` ["  context: in greeting, declared at 3:1 as String"]
    it "says when the binding has no signature" $ do
      name <- ok (mkIdentifier "flag")
      e <- sampleExplanation Nothing (InUnsignedBinding name)
      T.lines (renderExplanation e)
        `shouldContain` ["  context: in flag, which has no type signature"]
    it "puts the divergence before the context" $ do
      l <- ok (mkTypeText "Int")
      r <- ok (mkTypeText "[Char]")
      d <- ok (mkDivergence l r)
      c <- signedContext
      e <- sampleExplanation (Just d) c
      T.lines (renderExplanation e)
        `shouldBe` [ "type mismatch at 4:12",
                     "  expected: String",
                     "    actual: Int",
                     "  could not match: Int with [Char]",
                     "  context: in greeting, declared at 3:1 as String"
                   ]
    prop "has three lines, plus one for a divergence and one for a context" $
      forAll genExplanation $ \e ->
        length (T.lines (renderExplanation e))
          === 3 + fromEnum (isJust (explanationDivergence e)) + contextLineCount (explanationContext e)
  where
    contextLineCount :: Context -> Int
    contextLineCount = \case
      OutsideBinding -> 0
      _ -> 1
