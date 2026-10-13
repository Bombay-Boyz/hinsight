module HInsight.Tadka.LocateSpec (spec) where

import Data.Text (Text)
import Data.Text qualified as T
import HInsight.Source (Position, mkColumn, mkLine, mkPosition, mkSpan)
import HInsight.Tadka.Locate (offsetAt, offsetsOf)
import HInsight.Tadka.Support (ok)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

at :: Int -> Int -> IO Position
at l c = do
  line <- ok (mkLine l)
  col <- ok (mkColumn c)
  pure (mkPosition line col)

spec :: Spec
spec = describe "offsetAt" $ do
  it "puts the first position at offset 0" $
    at 1 1 >>= \p -> offsetAt "abc" p `shouldBe` Just 0
  it "counts the newline of every earlier line" $
    at 2 2 >>= \p -> offsetAt "ab\ncd\n" p `shouldBe` Just 4
  it "knows the empty line after a final newline" $
    at 2 1 >>= \p -> offsetAt "ab\n" p `shouldBe` Just 3
  it "knows the position just past the end of a line" $
    at 1 3 >>= \p -> offsetAt "ab\ncd" p `shouldBe` Just 2
  it "refuses a line the text does not have" $
    at 3 1 >>= \p -> offsetAt "ab\ncd" p `shouldBe` Nothing
  it "refuses a column past the end of its line" $
    at 1 4 >>= \p -> offsetAt "ab\ncd" p `shouldBe` Nothing
  it "counts a character outside ASCII as one column" $
    at 1 3 >>= \p -> offsetAt "\955x y" p `shouldBe` Just 2
  it "moves a tab to the next multiple of eight plus one" $ do
    at 1 9 >>= \p -> offsetAt "\tx" p `shouldBe` Just 1
    at 1 10 >>= \p -> offsetAt "\tx" p `shouldBe` Just 2
  it "refuses a column inside the width of a tab" $
    at 1 5 >>= \p -> offsetAt "\tx" p `shouldBe` Nothing
  it "advances a tab after other characters to the next stop" $
    at 1 9 >>= \p -> offsetAt "ab\tx" p `shouldBe` Just 3
  it "measures a span in characters" $ do
    start <- at 1 9
    end <- at 1 12
    sp <- ok (mkSpan start end)
    offsetsOf "\tfoo bar" sp `shouldBe` Just (1, 3)
  prop "finds the offset of every position that GHC's own counting produces" $
    forAll genSource $ \src ->
      forAll (choose (0, T.length src)) $ \offset ->
        let (l, c) = positionAt src offset
         in ioProperty (at l c >>= \p -> pure (offsetAt src p === Just offset))

-- | Text made of the characters that matter to column counting.
genSource :: Gen Text
genSource = T.pack <$> listOf (elements "ab \t\n\955")

-- | The line and column GHC gives the character at this offset, counted by
-- scanning the text from the start with GHC's rule, written independently of
-- the code under test.
positionAt :: Text -> Int -> (Int, Int)
positionAt src offset = T.foldl' step (1, 1) (T.take offset src)
  where
    step :: (Int, Int) -> Char -> (Int, Int)
    step (l, c) = \case
      '\n' -> (l + 1, 1)
      '\t' -> (l, 8 * ((c - 1) `div` 8 + 1) + 1)
      _ -> (l, c + 1)
