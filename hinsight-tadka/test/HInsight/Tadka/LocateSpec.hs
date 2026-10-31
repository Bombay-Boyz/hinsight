module HInsight.Tadka.LocateSpec (spec) where

import Data.Text (Text)
import Data.Text qualified as T
import HInsight.Source (Position, mkColumn, mkLine, mkPosition, mkSpan)
import HInsight.Tadka.Locate (offsetsOf)
import HInsight.Tadka.Support (ok)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

at :: Int -> Int -> IO Position
at l c = do
  line <- ok (mkLine l)
  col <- ok (mkColumn c)
  pure (mkPosition line col)

spanText :: Text -> (Int, Int) -> (Int, Int) -> IO (Maybe (Int, Int))
spanText src (l1, c1) (l2, c2) = do
  start <- at l1 c1
  end <- at l2 c2
  sp <- ok (mkSpan start end)
  pure (offsetsOf src sp)

spec :: Spec
spec = describe "offsetsOf" $ do
  it "measures a span in characters" $
    spanText "abc def" (1, 5) (1, 8) `shouldReturn` Just (4, 3)
  it "measures a span that starts after a tab by the tab's width" $
    spanText "\tfoo bar" (1, 9) (1, 12) `shouldReturn` Just (1, 3)
  it "measures a span across lines" $
    spanText "ab\ncd\n" (1, 2) (2, 2) `shouldReturn` Just (1, 3)
  it "counts a character outside ASCII as one column" $
    spanText "\955x y" (1, 3) (1, 4) `shouldReturn` Just (2, 1)
  it "refuses a span with an end the text does not have" $
    spanText "ab" (1, 1) (1, 9) `shouldReturn` Nothing
  prop "gives the offset and length of every span GHC's own counting produces" $
    forAll genSource $ \src ->
      forAll (choose (0, T.length src)) $ \a ->
        forAll (choose (a, T.length src)) $ \b ->
          let (l1, c1) = positionAt src a
              (l2, c2) = positionAt src b
           in ioProperty (spanText src (l1, c1) (l2, c2) >>= \r -> pure (r === Just (a, b - a)))

genSource :: Gen Text
genSource = T.pack <$> listOf (elements "ab \t\n\955")

-- | The line and column GHC gives the character at this offset, counted by
-- scanning the text from the start with GHC's rule.
positionAt :: Text -> Int -> (Int, Int)
positionAt src offset = T.foldl' step (1, 1) (T.take offset src)
  where
    step :: (Int, Int) -> Char -> (Int, Int)
    step (l, c) = \case
      '\n' -> (l + 1, 1)
      '\t' -> (l, 8 * ((c - 1) `div` 8 + 1) + 1)
      _ -> (l, c + 1)
