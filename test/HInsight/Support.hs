-- |
-- Module      : HInsight.Support
-- Description : Total generators and helpers shared by the specs.
--
-- Every generator builds values through the smart constructors, so a
-- generated value is valid by construction; 'suchThatMap' keeps the
-- generators total without partial functions.
module HInsight.Support
  ( rightToMaybe,
    ok,
    genLine,
    genColumn,
    genPosition,
    genSpan,
    genTypeText,
    genIdentifier,
    genHoleFit,
    genDivergence,
    genExplanation,
  )
where

import Data.Text qualified as T
import HInsight.Explanation
import HInsight.Hole
import HInsight.Source
import Test.QuickCheck

-- | Discard the error of an 'Either'.
rightToMaybe :: Either e a -> Maybe a
rightToMaybe = either (const Nothing) Just

-- | Unwrap a 'Right' inside a test, failing the test with the error otherwise.
ok :: (Show e) => Either e a -> IO a
ok = either (fail . show) pure

genLine :: Gen Line
genLine = suchThatMap (chooseInt (1, 1_000_000)) (rightToMaybe . mkLine)

genColumn :: Gen Column
genColumn = suchThatMap (chooseInt (1, 1_000_000)) (rightToMaybe . mkColumn)

genPosition :: Gen Position
genPosition = mkPosition <$> genLine <*> genColumn

genSpan :: Gen Span
genSpan = suchThatMap ((,) <$> genPosition <*> genPosition) (\(a, b) -> rightToMaybe (mkSpan (min a b) (max a b)))

-- | Rendered types: letters, spaces and type punctuation, never blank.
genTypeText :: Gen TypeText
genTypeText = suchThatMap (T.pack <$> listOf1 (elements "abcXYZ ->[]()")) (rightToMaybe . mkTypeText)

genIdentifier :: Gen Identifier
genIdentifier = suchThatMap (T.pack <$> listOf1 (elements (['a' .. 'z'] <> "_'"))) (rightToMaybe . mkIdentifier)

genRefinement :: Gen RefinementLevel
genRefinement = suchThatMap (chooseInt (0, 5)) (rightToMaybe . mkRefinementLevel)

genHoleFit :: Gen HoleFit
genHoleFit =
  HoleFit
    <$> genIdentifier
    <*> genTypeText
    <*> genRefinement
    <*> elements [minBound .. maxBound]
    <*> resize 3 (listOf genTypeText)

genDivergence :: Gen Divergence
genDivergence = suchThatMap ((,) <$> genTypeText <*> genTypeText) (rightToMaybe . uncurry mkDivergence)

genExplanation :: Gen Explanation
genExplanation =
  Explanation
    <$> genSpan
    <*> genTypeText
    <*> genTypeText
    <*> oneof [pure Nothing, Just <$> genDivergence]
    <*> (Origin . unTypeText <$> genTypeText)
