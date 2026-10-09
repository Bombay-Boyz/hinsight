module HInsight.IdentifierSpec (spec) where

import HInsight.Error (DomainError (..))
import HInsight.Identifier
import HInsight.Support (genIdentifier)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

spec :: Spec
spec = describe "mkIdentifier" $ do
  it "rejects the empty text" $
    mkIdentifier "" `shouldBe` Left (BlankText "identifier")
  it "rejects spaces only" $
    mkIdentifier "  " `shouldBe` Left (BlankText "identifier")
  it "rejects tabs and newlines only" $
    mkIdentifier "\t\n" `shouldBe` Left (BlankText "identifier")
  it "accepts a single character" $
    (unIdentifier <$> mkIdentifier "x") `shouldBe` Right "x"
  it "trims the ends" $
    (unIdentifier <$> mkIdentifier " map ") `shouldBe` Right "map"
  prop "is unchanged when rebuilt from its own text" $
    forAll genIdentifier $
      \i -> mkIdentifier (unIdentifier i) === Right i
