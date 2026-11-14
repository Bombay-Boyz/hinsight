-- |
-- Module      : HInsight.MoveVerifySpec
-- Description : The gate a move passes: GHC checks the files, and unused imports are pruned only when GHC agrees.
module HInsight.MoveVerifySpec (spec) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import HInsight.Edit (applyEditSet)
import HInsight.Error (InsightError (..), RefactorError (..))
import HInsight.Graph (mkModuleName)
import HInsight.Identifier (mkIdentifier)
import HInsight.Refactor.Move (Anchor (..), ExportClose (..), MoveFacts (..))
import HInsight.Refactor.Verify
import HInsight.Source (Position, SourceFile, Span, mkColumn, mkLine, mkPosition, mkSourceFile, mkSpan)
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
      "helper :: String -> String",
      "helper = map toUpper",
      "",
      "keep :: Int -> Int",
      "keep n = n + length (M.toList (moved [n]))"
    ]

sourceB :: Text
sourceB = "module B (other) where\n\nimport Data.List (sort)\n\nother :: [Int] -> [Int]\nother = sort\n"

facts :: IO MoveFacts
facts = do
  a <- ok (mkSourceFile "A.hs")
  b <- ok (mkSourceFile "B.hs")
  m <- ok (mkModuleName "B")
  n <- ok (mkIdentifier "moved")
  def <- spanOf (8, 1) (9, 50)
  imports <- sequence [spanOf (3, 1) (3, 27), spanOf (4, 1) (4, 26), spanOf (5, 1) (5, 31)]
  fromAnchor <- AfterImport <$> at (5, 31)
  toImports <- sequence [spanOf (3, 1) (3, 24)]
  toAnchor <- AfterImport <$> at (3, 24)
  close <- at (1, 16)
  pure (MoveFacts n a b m def imports fromAnchor toImports toAnchor (Just (ExportClose (close, True))) True)

-- | What each import is used for, to tell when an import is unused.
tokens :: [(Text, Text)]
tokens = [("import Data.Char (toUpper)", "toUpper"), ("import Data.List (sortOn)", "sortOn"), ("import qualified Data.Map as M", "M.")]

-- | A checker that behaves like GHC would for these two modules: an import
-- is unused when nothing below the imports uses what it brings in, and the
-- name @moved@ is not in scope in A without an import of B.
checkerFor :: Text -> MoveFacts -> [Diagnostic] -> (Map SourceFile Text -> [Diagnostic]) -> MoveChecker IO
checkerFor original fs baselineErrors extra = MoveChecker (fmap Right . answer)
  where
    answer :: Map SourceFile Text -> IO MoveChecked
    answer texts = do
      unused <- concat <$> traverse (uncurry unusedIn) (M.toList texts)
      let changed = M.lookup (moveFrom fs) texts /= Just original
      pure (MoveChecked ((if changed then [] else baselineErrors) <> extra texts) unused)
    unusedIn :: SourceFile -> Text -> IO [Diagnostic]
    unusedIn file text = sequence [diagnosticAt file n "The import is redundant" | (n, l) <- zip [1 ..] ls, Just token <- [lookup l tokens], not (any (T.isInfixOf token) (filter (not . T.isPrefixOf "import") ls))]
      where
        ls = T.lines text
    diagnosticAt :: SourceFile -> Int -> Text -> IO Diagnostic
    diagnosticAt file n message = do
      sp <- spanOf (n, 1) (n, 10)
      pure (Diagnostic file (Just sp) message)

plain :: MoveFacts -> MoveChecker IO
plain fs = checkerFor sourceA fs [] (const [])

planned :: MoveChecker IO -> MoveFacts -> IO (Either InsightError VerifiedPlan)
planned checker = verifyMove checker sourceA sourceB

applied :: MoveFacts -> VerifiedPlan -> IO (Text, Text)
applied fs plan = case applyEditSet (\f -> if f == moveFrom fs then Just sourceA else Just sourceB) (planEdits plan) of
  Left e -> fail (show e)
  Right m -> pure (M.findWithDefault sourceA (moveFrom fs) m, M.findWithDefault sourceB (moveTo fs) m)

spec :: Spec
spec = describe "verifyMove" $ do
  it "leaves out the imports GHC calls unused after the move, in both modules" $ do
    fs <- facts
    Right plan <- planned (plain fs) fs
    (a, b) <- applied fs plan
    a `shouldSatisfy` (not . T.isInfixOf "sortOn")
    a `shouldSatisfy` T.isInfixOf "import Data.Char (toUpper)\nimport qualified Data.Map as M\nimport B (moved)\n"
    b `shouldSatisfy` (not . T.isInfixOf "toUpper")
    b `shouldSatisfy` T.isInfixOf "import Data.List (sortOn)\nimport qualified Data.Map as M\n"
    planNotes plan `shouldBe` []
  it "refuses when the files did not compile before the move" $ do
    fs <- facts
    sp <- spanOf (1, 1) (1, 5)
    let broken = Diagnostic (moveFrom fs) (Just sp) "boom"
    planned (checkerFor sourceA fs [broken] (const [])) fs `shouldReturn` Left (RefactorFailure (FileDoesNotCompile 1))
  it "refuses, with GHC's message, a move after which the files do not compile" $ do
    fs <- facts
    sp <- spanOf (1, 1) (1, 5)
    let missing texts = [Diagnostic (moveFrom fs) (Just sp) "Variable not in scope: moved" | M.lookup (moveFrom fs) texts /= Just sourceA, maybe True (not . T.isInfixOf "import B") (M.lookup (moveFrom fs) texts)]
    planned (checkerFor sourceA fs [] missing) fs {moveKeepsImport = False}
      `shouldReturn` Left (RefactorFailure (VerificationFailed "Variable not in scope: moved"))
  it "keeps an import GHC called unused when GHC will not accept the files without it" $ do
    fs <- facts
    sp <- spanOf (1, 1) (1, 5)
    let needsChar texts = [Diagnostic (moveTo fs) (Just sp) "Not in scope: toUpper" | M.lookup (moveTo fs) texts /= Just sourceB, maybe False (not . T.isInfixOf "import Data.Char") (M.lookup (moveTo fs) texts)]
        stubborn = checkerFor sourceA fs [] needsChar
    Right plan <- planned stubborn fs
    (a, b) <- applied fs plan
    b `shouldSatisfy` T.isInfixOf "import Data.Char (toUpper)"
    a `shouldSatisfy` (not . T.isInfixOf "sortOn")
  it "does not prune an import that was already unused before the move" $ do
    fs <- facts
    let unusedAlready = T.replace "helper = map toUpper" "helper = id" sourceA
    Right plan <- verifyMove (checkerFor unusedAlready fs [] (const [])) unusedAlready sourceB fs
    case applyEditSet (\f -> if f == moveFrom fs then Just unusedAlready else Just sourceB) (planEdits plan) of
      Left e -> fail (show e)
      Right m -> M.findWithDefault "" (moveFrom fs) m `shouldSatisfy` T.isInfixOf "import Data.Char (toUpper)"
  it "reports as notes the places in other files GHC says still need a fix" $ do
    fs <- facts
    other <- ok (mkSourceFile "C.hs")
    sp <- spanOf (3, 18) (3, 23)
    let afterMove texts = [Diagnostic other (Just sp) "Module ‘A’ does not export ‘moved’." | M.lookup (moveFrom fs) texts /= Just sourceA]
    Right plan <- planned (checkerFor sourceA fs [] afterMove) fs
    planNotes plan `shouldBe` ["GHC reports a place still to fix: C.hs:3:18: Module ‘A’ does not export ‘moved’."]
