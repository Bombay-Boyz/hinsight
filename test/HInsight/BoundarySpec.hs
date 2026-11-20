-- |
-- Module      : HInsight.BoundarySpec
-- Description : Edits at the edges of a text: tabs, wide characters, line endings, empty and very large texts.
module HInsight.BoundarySpec (spec) where

import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import HInsight.Edit
import HInsight.Source (Position, SourceFile, Span, mkColumn, mkLine, mkPosition, mkSourceFile, mkSpan)
import HInsight.Support (ok)
import Test.Hspec

at :: Int -> Int -> IO Position
at l c = mkPosition <$> ok (mkLine l) <*> ok (mkColumn c)

spanOf :: (Int, Int) -> (Int, Int) -> IO Span
spanOf (l1, c1) (l2, c2) = do
  a <- at l1 c1
  b <- at l2 c2
  ok (mkSpan a b)

file :: IO SourceFile
file = ok (mkSourceFile "A.hs")

change :: Text -> (Int, Int) -> (Int, Int) -> Text -> IO (Either EditError Text)
change src a b new = do
  f <- file
  sp <- spanOf a b
  e <- ok (mkEdit f sp new)
  set <- ok (mkEditSet [e])
  pure (M.findWithDefault src f <$> applyEditSet (\g -> if g == f then Just src else Nothing) set)

spec :: Spec
spec = describe "Boundaries" $ do
  it "inserts into an empty text at line 1 column 1" $
    change "" (1, 1) (1, 1) "x" >>= (`shouldBe` Right "x")
  it "inserts at the very end of a text with no final newline" $
    change "ab" (1, 3) (1, 3) "c" >>= (`shouldBe` Right "abc")
  it "inserts on the empty last line after a final newline" $
    change "ab\n" (2, 1) (2, 1) "c" >>= (`shouldBe` Right "ab\nc")
  it "refuses a line past the end" $
    change "ab" (2, 1) (2, 1) "c" >>= (`shouldSatisfy` isLeft')
  it "refuses a column past the end of its line" $
    change "ab\ncd" (1, 4) (1, 4) "c" >>= (`shouldSatisfy` isLeft')
  it "counts a tab as GHC does: the column after a tab at the start is 9" $
    change "\tx" (1, 9) (1, 10) "y" >>= (`shouldBe` Right "\ty")
  it "refuses a column inside the width of a tab" $
    change "\tx" (1, 5) (1, 5) "y" >>= (`shouldSatisfy` isLeft')
  it "counts a character outside the basic plane as one column" $
    change "\x1F600x" (1, 2) (1, 3) "y" >>= (`shouldBe` Right "\x1F600y")
  it "counts a combining character as a column of its own" $
    change "e\x301x" (1, 3) (1, 4) "y" >>= (`shouldBe` Right "e\x301y")
  it "keeps a carriage return at the end of an edited line" $
    change "ab\r\ncd\r\n" (1, 1) (1, 3) "xy" >>= (`shouldBe` Right "xy\r\ncd\r\n")
  it "replaces the end of a CRLF line without touching the next line" $
    change "ab\r\ncd" (2, 1) (2, 3) "z" >>= (`shouldBe` Right "ab\r\nz")
  it "handles a text of a hundred thousand lines" $ do
    let src = T.unlines (replicate 100000 "x = 1")
    r <- change src (100000, 1) (100000, 2) "y"
    fmap (T.takeEnd 6) r `shouldBe` Right "y = 1\n"
  it "handles a single line of a million characters" $ do
    let src = T.replicate 1000000 "a"
    r <- change src (1, 1000000) (1, 1000001) "b"
    fmap T.length r `shouldBe` Right 1000000
  where
    isLeft' :: Either a b -> Bool
    isLeft' = either (const True) (const False)
