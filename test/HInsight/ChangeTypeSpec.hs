-- |
-- Module      : HInsight.ChangeTypeSpec
-- Description : The edit that changes a signature, and the gate GHC's answer passes through.
module HInsight.ChangeTypeSpec (spec) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import HInsight.Edit (applyEditSet)
import HInsight.Error (InsightError (..), RefactorError (..))
import HInsight.Identifier (mkIdentifier)
import HInsight.Refactor.ChangeType (ChangeFacts (..), planChangeType)
import HInsight.Refactor.Verify
import HInsight.Source (Position, SourceFile, Span, mkColumn, mkLine, mkPosition, mkSourceFile, mkSpan)
import HInsight.Support (ok)
import HInsight.TypeText (mkTypeText)
import Test.Hspec

at :: (Int, Int) -> IO Position
at (l, c) = mkPosition <$> ok (mkLine l) <*> ok (mkColumn c)

spanOf :: (Int, Int) -> (Int, Int) -> IO Span
spanOf a b = do
  p <- at a
  q <- at b
  ok (mkSpan p q)

source :: Text
source = "module A (count, bare) where\n\ncount :: Int -> Int\ncount n = n + 1\n\nbare = 7\n"

signed :: IO ChangeFacts
signed = do
  file <- ok (mkSourceFile "A.hs")
  name <- ok (mkIdentifier "count")
  sig <- spanOf (3, 10) (3, 20)
  binding <- spanOf (4, 1) (4, 16)
  pure (ChangeFacts name file (Just sig) binding)

unsigned :: IO ChangeFacts
unsigned = do
  file <- ok (mkSourceFile "A.hs")
  name <- ok (mkIdentifier "bare")
  binding <- spanOf (6, 1) (6, 9)
  pure (ChangeFacts name file Nothing binding)

rewritten :: ChangeFacts -> Text -> IO Text
rewritten facts wanted = do
  t <- ok (mkTypeText wanted)
  case planChangeType facts t of
    Left e -> fail (show e)
    Right set -> case applyEditSet (const (Just source)) set of
      Left e -> fail (show e)
      Right m -> pure (M.findWithDefault source (changeFile facts) m)

-- | A checker that has the given errors once the text has changed.
checkerAfter :: [Diagnostic] -> [Diagnostic] -> ChangeChecker IO
checkerAfter beforeErrors afterErrors = ChangeChecker (answer beforeErrors) (answer afterErrors)
  where
    answer :: [Diagnostic] -> Map SourceFile Text -> IO (Either InsightError MoveChecked)
    answer errs _ = pure (Right (MoveChecked errs []))

spec :: Spec
spec = do
  describe "planChangeType" $ do
    it "replaces the type of the signature" $ do
      facts <- signed
      rewritten facts "Integer -> Integer" `shouldReturn` "module A (count, bare) where\n\ncount :: Integer -> Integer\ncount n = n + 1\n\nbare = 7\n"
    it "puts a signature above a binding that has none" $ do
      facts <- unsigned
      rewritten facts "Int" `shouldReturn` "module A (count, bare) where\n\ncount :: Int -> Int\ncount n = n + 1\n\nbare :: Int\nbare = 7\n"
  describe "verifyChangeType" $ do
    it "refuses when the file does not typecheck as it is" $ do
      facts <- signed
      t <- ok (mkTypeText "Integer -> Integer")
      let d = Diagnostic (changeFile facts) Nothing "bad"
      verifyChangeType (checkerAfter [d] []) source facts t `shouldReturn` Left (RefactorFailure (FileDoesNotCompile 1))
    it "refuses when GHC finds an error in the changed file" $ do
      facts <- signed
      t <- ok (mkTypeText "Integer -> Integer")
      let d = Diagnostic (changeFile facts) Nothing "Couldn't match"
      verifyChangeType (checkerAfter [] [d]) source facts t `shouldReturn` Left (RefactorFailure (VerificationFailed "Couldn't match"))
    it "gives an error in another file as a note" $ do
      facts <- signed
      t <- ok (mkTypeText "Integer -> Integer")
      other <- ok (mkSourceFile "B.hs")
      sp <- spanOf (6, 8) (6, 16)
      let d = Diagnostic other (Just sp) "Couldn't match type"
      Right plan <- verifyChangeType (checkerAfter [] [d]) source facts t
      planNotes plan `shouldBe` ["GHC reports a place still to fix: B.hs:6:8: Couldn't match type"]
    it "has no notes when GHC reports nothing" $ do
      facts <- signed
      t <- ok (mkTypeText "Int -> Int")
      Right plan <- verifyChangeType (checkerAfter [] []) source facts t
      planNotes plan `shouldBe` []
