module HInsight.MoveSpec (spec) where

import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import HInsight.Edit (applyEditSet)
import HInsight.Error (InsightError (..), RefactorError (..))
import HInsight.Graph (mkModuleName)
import HInsight.Identifier (mkIdentifier)
import HInsight.Refactor.Move
import HInsight.Source (Position, Span, mkColumn, mkLine, mkPosition, mkSourceFile, mkSpan)
import HInsight.Support (ok)
import Test.Hspec

at :: (Int, Int) -> IO Position
at (l, c) = mkPosition <$> ok (mkLine l) <*> ok (mkColumn c)

spanOf :: (Int, Int) -> (Int, Int) -> IO Span
spanOf a b = do
  p <- at a
  q <- at b
  ok (mkSpan p q)

sourceA :: Text
sourceA =
  T.unlines
    [ "module A (keep, helper) where",
      "",
      "import Data.Char (toUpper)",
      "import Data.List (sortOn)",
      "import qualified Data.Map as M",
      "",
      "-- | Pairs each number with its place in descending order.",
      "moved :: [Int] -> M.Map Int Int",
      "moved xs = M.fromList (zip (sortOn negate xs) xs)",
      "",
      "{- | A block doc. -}",
      "helper :: String -> String",
      "helper = map toUpper",
      "",
      "keep :: Int -> Int",
      "keep n = n + length (M.toList (moved [n]))"
    ]

sourceB :: Text
sourceB = "module B (other) where\n\nimport Data.List (sort)\n\nother :: [Int] -> [Int]\nother = sort\n"

facts :: Text -> (Int, Int) -> (Int, Int) -> IO MoveFacts
facts name from to = do
  a <- ok (mkSourceFile "A.hs")
  b <- ok (mkSourceFile "B.hs")
  m <- ok (mkModuleName "B")
  n <- ok (mkIdentifier name)
  def <- spanOf from to
  imports <- sequence [spanOf (3, 1) (3, 27), spanOf (4, 1) (4, 26), spanOf (5, 1) (5, 31)]
  fromAnchor <- AfterImport <$> at (5, 31)
  toImports <- sequence [spanOf (3, 1) (3, 24)]
  toAnchor <- AfterImport <$> at (3, 24)
  close <- at (1, 16)
  pure (MoveFacts n a b m def imports fromAnchor toImports toAnchor (Just (ExportClose (close, True))) True)

movedFacts :: IO MoveFacts
movedFacts = facts "moved" (8, 1) (9, 50)

run :: [Span] -> Text -> Text -> MoveFacts -> IO (Either InsightError (Text, Text))
run dropped fromSrc toSrc fs = case planMove dropped fromSrc toSrc fs of
  Left e -> pure (Left e)
  Right set -> case applyEditSet (\f -> if f == moveFrom fs then Just fromSrc else Just toSrc) set of
    Left e -> fail (show e)
    Right m -> pure (Right (M.findWithDefault fromSrc (moveFrom fs) m, M.findWithDefault toSrc (moveTo fs) m))

spec :: Spec
spec = describe "planMove" $ do
  it "moves a definition with its Haddock comment, its imports, and an export" $ do
    fs <- movedFacts
    run [] sourceA sourceB fs
      `shouldReturn` Right
        ( T.unlines
            [ "module A (keep, helper) where",
              "",
              "import Data.Char (toUpper)",
              "import Data.List (sortOn)",
              "import qualified Data.Map as M",
              "import B (moved)",
              "",
              "{- | A block doc. -}",
              "helper :: String -> String",
              "helper = map toUpper",
              "",
              "keep :: Int -> Int",
              "keep n = n + length (M.toList (moved [n]))"
            ],
          T.unlines
            [ "module B (other, moved) where",
              "",
              "import Data.List (sort)",
              "import Data.Char (toUpper)",
              "import Data.List (sortOn)",
              "import qualified Data.Map as M",
              "",
              "other :: [Int] -> [Int]",
              "other = sort",
              "",
              "-- | Pairs each number with its place in descending order.",
              "moved :: [Int] -> M.Map Int Int",
              "moved xs = M.fromList (zip (sortOn negate xs) xs)"
            ]
        )
  it "leaves out the imports it is told to drop" $ do
    fs <- movedFacts
    unused <- spanOf (3, 1) (3, 27)
    Right (_, b) <- run [unused] sourceA sourceB fs
    b `shouldSatisfy` (not . T.isInfixOf "Data.Char")
    b `shouldSatisfy` T.isInfixOf "import qualified Data.Map as M"
  it "does not copy an import the target already has, whatever its spacing" $ do
    fs <- movedFacts
    let withMap = "module B (other) where\n\nimport Data.List (sort)\nimport qualified  Data.Map   as M\n\nother = sort\n"
    existing <- sequence [spanOf (3, 1) (3, 24), spanOf (4, 1) (4, 34)]
    anchor <- AfterImport <$> at (4, 34)
    Right (_, b) <- run [] sourceA withMap fs {moveToImports = existing, moveToAnchor = anchor}
    T.count "Data.Map" b `shouldBe` 1
  it "adds no import to the source and no export to the target when the source no longer needs the name" $ do
    fs <- movedFacts
    Right (a, b) <- run [] sourceA sourceB fs {moveKeepsImport = False}
    a `shouldSatisfy` (not . T.isInfixOf "import B")
    b `shouldSatisfy` T.isInfixOf "module B (other) where"
  it "adds no export when the target has no export list" $ do
    fs <- movedFacts
    Right (_, b) <- run [] sourceA "module B where\n\nimport Data.List (sort)\n" fs {moveToExports = Nothing}
    b `shouldSatisfy` T.isInfixOf "module B where"
  it "starts an export list that is empty" $ do
    fs <- movedFacts
    close <- at (1, 11)
    Right (_, b) <- run [] sourceA "module B () where\n\nimport Data.List (sort)\n" fs {moveToExports = Just (ExportClose (close, False))}
    b `shouldSatisfy` T.isInfixOf "module B (moved) where"
  it "puts the copied imports after the module header when the target has none" $ do
    fs <- movedFacts
    anchor <- AfterModuleHeader <$> at (1, 15)
    Right (_, b) <- run [] sourceA "module B where\n" fs {moveToImports = [], moveToAnchor = anchor, moveToExports = Nothing}
    T.lines b `shouldBe` ["module B where", "", "import Data.Char (toUpper)", "import Data.List (sortOn)", "import qualified Data.Map as M", "", "-- | Pairs each number with its place in descending order.", "moved :: [Int] -> M.Map Int Int", "moved xs = M.fromList (zip (sortOn negate xs) xs)"]
  it "moves a block Haddock comment with its definition" $ do
    fs <- facts "helper" (12, 1) (13, 21)
    Right (a, b) <- run [] sourceA sourceB fs
    a `shouldSatisfy` (not . T.isInfixOf "A block doc")
    b `shouldSatisfy` T.isInfixOf "{- | A block doc. -}\nhelper :: String -> String\nhelper = map toUpper\n"
  it "leaves a plain comment above the definition where it is" $ do
    let src = "module A where\n\n-- plain note\nf :: Int\nf = 1\n"
    fs <- facts "f" (4, 1) (5, 6)
    header <- at (1, 15)
    Right (a, b) <- run [] src sourceB fs {moveFromImports = [], moveFromAnchor = AfterModuleHeader header, moveKeepsImport = False}
    a `shouldBe` "module A where\n\n-- plain note\n"
    b `shouldSatisfy` T.isSuffixOf "other = sort\n\nf :: Int\nf = 1\n"
  it "moves the whole of a multi-line Haddock comment and nothing above it" $ do
    let src = "module A where\n\n-- note\n-- | First line.\n-- Second line.\nf :: Int\nf = 1\n\ng :: Int\ng = 2\n"
    fs <- facts "f" (6, 1) (7, 6)
    header <- at (1, 15)
    Right (a, b) <- run [] src sourceB fs {moveFromImports = [], moveFromAnchor = AfterModuleHeader header, moveKeepsImport = False}
    a `shouldBe` "module A where\n\n-- note\ng :: Int\ng = 2\n"
    b `shouldSatisfy` T.isSuffixOf "\n-- | First line.\n-- Second line.\nf :: Int\nf = 1\n"
  it "removes a definition that ends the file with no final newline" $ do
    let src = "module A where\n\nf :: Int\nf = 1"
    fs <- facts "f" (3, 1) (4, 6)
    header <- at (1, 15)
    Right (a, _) <- run [] src sourceB fs {moveFromImports = [], moveFromAnchor = AfterModuleHeader header, moveKeepsImport = False}
    a `shouldBe` "module A where\n\n"
  it "writes an operator in parentheses" $ do
    let src = "module A where\n\n(+++) :: Int -> Int -> Int\n(+++) = (+)\n"
    fs <- facts "+++" (3, 1) (4, 12)
    header <- at (1, 15)
    Right (a, b) <- run [] src sourceB fs {moveFromImports = [], moveFromAnchor = AfterModuleHeader header}
    a `shouldSatisfy` T.isInfixOf "import B ((+++))"
    b `shouldSatisfy` T.isInfixOf "module B (other, (+++)) where"
  it "refuses a definition that does not start at the first column" $ do
    fs <- facts "moved" (8, 2) (9, 50)
    run [] sourceA sourceB fs `shouldReturn` Left (RefactorFailure NotATopLevelDefinition)
  it "refuses a span the text does not have" $ do
    fs <- facts "moved" (80, 1) (90, 50)
    run [] sourceA sourceB fs `shouldReturn` Left (RefactorFailure SelectionNotInText)
