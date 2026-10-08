module HInsight.SourceSpec (spec) where

import Data.Either (isRight)
import HInsight.Error (DomainError (..))
import HInsight.Source
import HInsight.Support (genPosition, ok)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

positionAt :: Int -> Int -> IO Position
positionAt l c = mkPosition <$> ok (mkLine l) <*> ok (mkColumn c)

spec :: Spec
spec = do
  describe "mkSourceFile" $ do
    it "rejects the empty path" $
      mkSourceFile "" `shouldBe` Left (EmptyPath "source file")
    it "accepts a one-character path" $
      (sourceFilePath <$> mkSourceFile "a") `shouldBe` Right "a"
    prop "round-trips every non-empty path" $ \(NonEmpty p) ->
      (sourceFilePath <$> mkSourceFile p) === Right p

  describe "mkLine" $ do
    it "rejects zero, the value just below the minimum" $
      mkLine 0 `shouldBe` Left (NonPositiveLine 0)
    it "accepts one, the minimum" $
      (unLine <$> mkLine 1) `shouldBe` Right 1
    it "rejects the most negative Int" $
      mkLine minBound `shouldBe` Left (NonPositiveLine minBound)
    it "accepts the largest Int" $
      (unLine <$> mkLine maxBound) `shouldBe` Right maxBound
    prop "is accepted exactly when at least 1" $ \n ->
      isRight (mkLine n) === (n >= 1)

  describe "mkColumn" $ do
    it "rejects zero" $
      mkColumn 0 `shouldBe` Left (NonPositiveColumn 0)
    it "accepts one" $
      (unColumn <$> mkColumn 1) `shouldBe` Right 1
    prop "is accepted exactly when at least 1" $ \n ->
      isRight (mkColumn n) === (n >= 1)

  describe "mkSpan" $ do
    it "allows a zero-width span" $ do
      p <- positionAt 3 7
      (spanStart <$> mkSpan p p) `shouldBe` Right p
    it "rejects an end one column before the start" $ do
      s <- positionAt 3 7
      e <- positionAt 3 6
      mkSpan s e `shouldBe` Left (SpanEndsBeforeStart 3 7 3 6)
    it "rejects an earlier column on a later line only when the line is earlier" $ do
      s <- positionAt 2 1
      e <- positionAt 1 99
      mkSpan s e `shouldBe` Left (SpanEndsBeforeStart 2 1 1 99)
    it "accepts an earlier column when the end is on a later line" $ do
      s <- positionAt 1 99
      e <- positionAt 2 1
      isRight (mkSpan s e) `shouldBe` True
    prop "is accepted exactly when the start is not after the end" $
      forAll genPosition $ \a -> forAll genPosition $ \b ->
        isRight (mkSpan a b) === (a <= b)
    prop "keeps its endpoints" $
      forAll genPosition $ \a -> forAll genPosition $ \b ->
        let (lo, hi) = (min a b, max a b)
         in ((,) <$> fmap spanStart (mkSpan lo hi) <*> fmap spanEnd (mkSpan lo hi)) === Right (lo, hi)
