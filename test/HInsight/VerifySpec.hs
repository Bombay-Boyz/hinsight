module HInsight.VerifySpec (spec) where

import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import HInsight.Edit (applyEditSet)
import HInsight.Error (InsightError (..), RefactorError (..), SessionError (..))
import HInsight.Identifier (mkIdentifier)
import HInsight.Refactor (CallContext (..), ExtractFacts (..), mkFunctionName)
import HInsight.Refactor.Verify
import HInsight.Source (Position, Span, mkColumn, mkLine, mkPosition, mkSourceFile, mkSpan)
import HInsight.Support (ok)
import HInsight.TypeText (mkTypeText)
import Test.Hspec

at :: Int -> Int -> IO Position
at l c = mkPosition <$> ok (mkLine l) <*> ok (mkColumn c)

spanOf :: (Int, Int) -> (Int, Int) -> IO Span
spanOf (l1, c1) (l2, c2) = do
  a <- at l1 c1
  b <- at l2 c2
  ok (mkSpan a b)

source :: Text
source = "g :: Int -> Int\ng x = x + 1\n"

factsG :: IO ExtractFacts
factsG = do
  f <- ok (mkSourceFile "A.hs")
  sel <- spanOf (2, 7) (2, 12)
  decl <- spanOf (2, 1) (2, 12)
  x <- ok (mkIdentifier "x")
  pure (ExtractFacts f sel decl [x] CallIsWhole)

withSignature :: Text
withSignature = "g :: Int -> Int\ng x = inc x\n\ninc :: Int -> Int\ninc x =\n  x + 1\n"

withoutSignature :: Text
withoutSignature = "g :: Int -> Int\ng x = inc x\n\ninc x =\n  x + 1\n"

-- | A checker that records what it was asked and answers by the text.
checkerOf :: (Text -> Checked) -> IO (Checker IO, IO [Text])
checkerOf answer = do
  asked <- newIORef []
  pure (Checker (\t -> modifyIORef' asked (t :) >> pure (Right (answer t))), reverse <$> readIORef asked)

typed :: IO Checked
typed = do
  n <- ok (mkIdentifier "inc")
  t <- ok (mkTypeText "Int -> Int")
  pure (Checked [] (M.singleton n t))

result :: Checker IO -> IO (Either InsightError VerifiedPlan)
result checker = do
  facts <- factsG
  name <- either (fail . show) pure (mkFunctionName "inc")
  verifyExtract checker source facts name

textOf :: VerifiedPlan -> IO Text
textOf plan = do
  f <- ok (mkSourceFile "A.hs")
  texts <- either (fail . show) pure (applyEditSet (const (Just source)) (planEdits plan))
  maybe (fail "no text") pure (M.lookup f texts)

spec :: Spec
spec = describe "verifyExtract" $ do
  it "offers the plan with the signature GHC gave, after GHC accepted it" $ do
    good <- typed
    (checker, asked) <- checkerOf (const good)
    Right plan <- result checker
    textOf plan `shouldReturn` withSignature
    planNotes plan `shouldBe` []
    asked `shouldReturn` [withoutSignature, withSignature]
  it "refuses with GHC's first error when the plan without a signature does not compile" $ do
    (checker, _) <- checkerOf (const (Checked ["A.hs:2:7: error: boom", "second"] M.empty))
    result checker `shouldReturn` Left (RefactorFailure (VerificationFailed "A.hs:2:7: error: boom"))
  it "offers the plan without a signature, and says so, when GHC rejects the signature it gave" $ do
    good <- typed
    (checker, _) <- checkerOf (\t -> if t == withSignature then Checked ["bad signature"] M.empty else good)
    Right plan <- result checker
    textOf plan `shouldReturn` withoutSignature
    planNotes plan `shouldSatisfy` (\ns -> length ns == 1 && all (T.isInfixOf "bad signature") ns)
  it "offers the plan without a signature, and says so, when GHC gave no type" $ do
    (checker, _) <- checkerOf (const (Checked [] M.empty))
    Right plan <- result checker
    textOf plan `shouldReturn` withoutSignature
    length (planNotes plan) `shouldBe` 1
  it "passes on a failure of the checker itself" $ do
    facts <- factsG
    name <- either (fail . show) pure (mkFunctionName "inc")
    let broken = Checker (const (pure (Left (SessionFailure (LibDirNotFound "/x")))))
    verifyExtract broken source facts name `shouldReturn` Left (SessionFailure (LibDirNotFound "/x"))
  it "does not ask GHC when the plan cannot be made" $ do
    facts <- factsG
    name <- either (fail . show) pure (mkFunctionName "x")
    (checker, asked) <- checkerOf (const (Checked [] M.empty))
    verifyExtract checker source facts name `shouldReturn` Left (RefactorFailure (NameClashesWithVariable "x"))
    asked `shouldReturn` []
