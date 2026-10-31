module HInsight.EditSpec (spec) where

import Data.List (sort)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import HInsight.Edit
import HInsight.Error (DomainError (..), renderDomainError)
import HInsight.Source (Position, SourceFile, Span, mkColumn, mkLine, mkPosition, mkSourceFile, mkSpan)
import HInsight.Support (ok)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

at :: Int -> Int -> IO Position
at l c = mkPosition <$> ok (mkLine l) <*> ok (mkColumn c)

spanOf :: (Int, Int) -> (Int, Int) -> IO Span
spanOf (l1, c1) (l2, c2) = do
  a <- at l1 c1
  b <- at l2 c2
  ok (mkSpan a b)

file :: String -> IO SourceFile
file = ok . mkSourceFile

editOf :: SourceFile -> (Int, Int) -> (Int, Int) -> Text -> IO Edit
editOf f a b t = spanOf a b >>= \sp -> ok (mkEdit f sp t)

apply :: SourceFile -> Text -> [Edit] -> IO (Either EditError Text)
apply f src es = do
  set <- ok (mkEditSet es)
  pure (M.findWithDefault src f <$> applyEditSet (\g -> if g == f then Just src else Nothing) set)

spec :: Spec
spec = describe "Edit" $ do
  describe "mkEdit" $ do
    it "keeps the file, the span and the text" $ do
      f <- file "A.hs"
      sp <- spanOf (1, 1) (1, 4)
      e <- ok (mkEdit f sp "x")
      (editFile e, editSpan e, editText e) `shouldBe` (f, sp, "x")
    it "refuses text that holds a NUL" $ do
      f <- file "A.hs"
      sp <- spanOf (1, 1) (1, 1)
      mkEdit f sp "a\NULb" `shouldBe` Left EditTextHasNul
  describe "mkEditSet" $ do
    it "refuses two overlapping edits and names where each starts" $ do
      f <- file "A.hs"
      a <- editOf f (1, 1) (1, 5) "x"
      b <- editOf f (1, 3) (1, 6) "y"
      mkEditSet [b, a] `shouldBe` Left (OverlappingEdits (1, 1) (1, 3))
    it "accepts edits that touch and edits of different files" $ do
      f <- file "A.hs"
      g <- file "B.hs"
      a <- editOf f (1, 1) (1, 3) "x"
      b <- editOf f (1, 3) (1, 5) "y"
      c <- editOf g (1, 2) (1, 4) "z"
      fmap (length . editSetEdits) (mkEditSet [c, b, a]) `shouldBe` Right 3
    it "accepts an insertion at the start of another edit, whatever the order" $ do
      f <- file "A.hs"
      ins <- editOf f (1, 2) (1, 2) "<"
      rep <- editOf f (1, 2) (1, 4) "x"
      mkEditSet [rep, ins] `shouldBe` mkEditSet [ins, rep]
      mkEditSet [rep, ins] `shouldSatisfy` either (const False) (const True)
    it "says what overlaps in words" $
      renderDomainError (OverlappingEdits (1, 1) (2, 3)) `shouldSatisfy` T.isInfixOf "1:1 and 2:3"
  describe "applyEditSet" $ do
    it "replaces a span" $ do
      f <- file "A.hs"
      e <- editOf f (1, 5) (1, 8) "bar"
      apply f "let foo = 1" [e] `shouldReturn` Right "let bar = 1"
    it "inserts at an empty span" $ do
      f <- file "A.hs"
      e <- editOf f (1, 4) (1, 4) "!"
      apply f "abc" [e] `shouldReturn` Right "abc!"
    it "applies edits on several lines" $ do
      f <- file "A.hs"
      a <- editOf f (1, 1) (1, 2) "X"
      b <- editOf f (3, 1) (3, 2) "Z"
      apply f "a\nb\nc\n" [b, a] `shouldReturn` Right "X\nb\nZ\n"
    it "puts two insertions at one point in the order of their text" $ do
      f <- file "A.hs"
      a <- editOf f (1, 2) (1, 2) "A"
      b <- editOf f (1, 2) (1, 2) "B"
      apply f "xy" [b, a] `shouldReturn` Right "xABy"
      apply f "xy" [a, b] `shouldReturn` Right "xABy"
    it "refuses text it was not given" $ do
      f <- file "A.hs"
      g <- file "B.hs"
      e <- editOf g (1, 1) (1, 1) "x"
      apply f "abc" [e] `shouldReturn` Left (NoTextFor g)
    it "refuses a position the text does not have, after the file changed" $ do
      f <- file "A.hs"
      e <- editOf f (3, 1) (3, 2) "x"
      p <- at 3 1
      apply f "a\n" [e] `shouldReturn` Left (PositionOutsideText f p)
    it "says which file and position in words" $ do
      f <- file "A.hs"
      p <- at 3 1
      renderEditError (PositionOutsideText f p) `shouldSatisfy` T.isInfixOf "3:1"
      renderEditError (NoTextFor f) `shouldSatisfy` T.isInfixOf "A.hs"
    it "moves a tab to the next stop before it places an edit" $ do
      f <- file "A.hs"
      e <- editOf f (1, 9) (1, 10) "Y"
      apply f "\tx" [e] `shouldReturn` Right "\tY"
  describe "offsetOf" $ do
    it "puts the first position at offset 0 and counts earlier newlines" $ do
      at 1 1 >>= \p -> offsetOf "abc" p `shouldBe` Just 0
      at 2 2 >>= \p -> offsetOf "ab\ncd\n" p `shouldBe` Just 4
    it "knows the empty line after a final newline" $
      at 2 1 >>= \p -> offsetOf "ab\n" p `shouldBe` Just 3
    it "refuses a line, a column and a tab stop the text does not have" $ do
      at 3 1 >>= \p -> offsetOf "ab\ncd" p `shouldBe` Nothing
      at 1 4 >>= \p -> offsetOf "ab\ncd" p `shouldBe` Nothing
      at 1 5 >>= \p -> offsetOf "\tx" p `shouldBe` Nothing
    prop "agrees with an independently written count of GHC's columns" $
      forAll genSource $ \src ->
        forAll (choose (0, T.length src)) $ \offset ->
          let (l, c) = positionAt src offset
           in ioProperty (at l c >>= \p -> pure (offsetOf src p === Just offset))
  describe "properties" $ do
    prop "no edits give no files" $
      forAll genSource $ \src -> ioProperty $ do
        set <- ok (mkEditSet [])
        pure (applyEditSet (const (Just src)) set === Right M.empty)
    prop "replacing each span by its own text leaves the text unchanged" $
      forAll genCase $ \(src, marks) -> ioProperty $ do
        f <- file "A.hs"
        es <- traverse (\(a, b, _) -> editOf f (1, a + 1) (1, b + 1) (slice a b src)) marks
        (=== Right src) <$> apply f src es
    prop "any order of disjoint edits gives one edit set and one result" $
      forAll genCase $ \(src, marks) ->
        forAll (shuffle marks) $ \shuffled -> ioProperty $ do
          f <- file "A.hs"
          let build = traverse (\(a, b, t) -> editOf f (1, a + 1) (1, b + 1) t)
          s <- build marks >>= ok . mkEditSet
          s' <- build shuffled >>= ok . mkEditSet
          pure (s === s' .&&. applyEditSet (const (Just src)) s === applyEditSet (const (Just src)) s')
    prop "the text outside every edited span is untouched and the rest is the new text" $
      forAll genCase $ \(src, marks) -> ioProperty $ do
        f <- file "A.hs"
        es <- traverse (\(a, b, t) -> editOf f (1, a + 1) (1, b + 1) t) marks
        (=== Right (expected src marks)) <$> apply f src es

slice :: Int -> Int -> Text -> Text
slice a b = T.take (b - a) . T.drop a

expected :: Text -> [(Int, Int, Text)] -> Text
expected src marks = T.concat (go 0 marks)
  where
    go :: Int -> [(Int, Int, Text)] -> [Text]
    go from [] = [T.drop from src]
    go from ((a, b, t) : rest) = slice from a src : t : go b rest

genCase :: Gen (Text, [(Int, Int, Text)])
genCase = do
  src <- T.pack <$> listOf (elements "abc")
  k <- choose (0, 4)
  cuts <- sort <$> vectorOf (2 * k) (choose (0, T.length src))
  texts <- vectorOf k (elements ["", "X", "YZ"])
  pure (src, zipWith (\(a, b) t -> (a, b, t)) (pairs cuts) texts)
  where
    pairs :: [Int] -> [(Int, Int)]
    pairs (a : b : rest) = (a, b) : pairs rest
    pairs _ = []

genSource :: Gen Text
genSource = T.pack <$> listOf (elements "ab \t\n\955")

positionAt :: Text -> Int -> (Int, Int)
positionAt src offset = T.foldl' step (1, 1) (T.take offset src)
  where
    step :: (Int, Int) -> Char -> (Int, Int)
    step (l, c) = \case
      '\n' -> (l + 1, 1)
      '\t' -> (l, 8 * ((c - 1) `div` 8 + 1) + 1)
      _ -> (l, c + 1)
