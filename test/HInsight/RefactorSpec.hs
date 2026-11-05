module HInsight.RefactorSpec (spec) where

import Data.Either (isRight)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import HInsight.Edit (applyEditSet)
import HInsight.Error (InsightError (..), RefactorError (..), renderError)
import HInsight.Identifier (Identifier, mkIdentifier)
import HInsight.Refactor
import HInsight.Source (Position, SourceFile, Span, mkColumn, mkLine, mkPosition, mkSourceFile, mkSpan)
import HInsight.Support (ok)
import HInsight.TypeText (mkTypeText)
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

file :: IO SourceFile
file = ok (mkSourceFile "A.hs")

ident :: Text -> IO Identifier
ident = ok . mkIdentifier

named :: Text -> IO FunctionName
named t = either (\e -> expectationFailure (show e) >> fail "name") pure (mkFunctionName t)

facts :: (Int, Int) -> (Int, Int) -> (Int, Int) -> (Int, Int) -> [Text] -> CallContext -> IO ExtractFacts
facts s1 s2 d1 d2 vars ctx = do
  f <- file
  sel <- spanOf s1 s2
  decl <- spanOf d1 d2
  vs <- traverse ident vars
  pure (ExtractFacts f sel decl vs ctx)

applied :: Text -> ExtractFacts -> Maybe Text -> Text -> IO (Either InsightError Text)
applied src fs sig name = do
  n <- named name
  t <- traverse (ok . mkTypeText) sig
  case planExtract src fs t n of
    Left e -> pure (Left e)
    Right set -> pure $ case applyEditSet (const (Just src)) set of
      Left _ -> Left (RefactorFailure SelectionNotInText)
      Right m -> Right (M.findWithDefault src (factsFile fs) m)

refusal :: Either InsightError a -> Maybe RefactorError
refusal = \case
  Left (RefactorFailure e) -> Just e
  _ -> Nothing

spec :: Spec
spec = describe "Refactor" $ do
  describe "mkFunctionName" $ do
    it "accepts variable names" $
      mapM_ (\t -> mkFunctionName t `shouldSatisfy` isRight) ["f", "twice", "_x", "go'", "x1", "xs_2"]
    it "refuses what is not a variable name" $
      mapM_ (\t -> mkFunctionName t `shouldBe` Left (InvalidFunctionName t)) ["", "Foo", "1x", "x y", "x-y", "(+)", "_", "case", "where", "let", "in"]
    prop "accepts a lowercase letter followed by exactly the characters a name may hold" $
      forAll (listOf (elements ("ab_'1 -" :: String))) $ \s ->
        isRight (mkFunctionName (T.pack ('x' : s))) === all (`elem` ("ab_'1" :: String)) s
  describe "planExtract" $ do
    it "extracts a part of an expression with its free variable and signature" $ do
      let src = "f :: Int -> Int -> Int\nf k n = let m = n * 2 in (m + k) * (m + k)\n"
      fs <- facts (2, 17) (2, 22) (2, 1) (2, 43) ["n"] CallIsPart
      applied src fs (Just "Int -> Int") "twice"
        `shouldReturn` Right "f :: Int -> Int -> Int\nf k n = let m = (twice n) in (m + k) * (m + k)\n\ntwice :: Int -> Int\ntwice n =\n  n * 2\n"
    it "leaves a whole right-hand side without parentheses" $ do
      let src = "g :: Int -> Int\ng x = x + 1\n"
      fs <- facts (2, 7) (2, 12) (2, 1) (2, 12) ["x"] CallIsWhole
      applied src fs Nothing "inc" `shouldReturn` Right "g :: Int -> Int\ng x = inc x\n\ninc x =\n  x + 1\n"
    it "extracts a multi-line selection at the same indentation" $ do
      let src = "g :: IO ()\ng = do\n  putStrLn \"a\"\n  mapM_ print\n    [1, 2, 3 :: Int]\n"
      fs <- facts (4, 3) (5, 21) (2, 1) (5, 21) [] CallIsPart
      applied src fs (Just "IO ()") "printAll"
        `shouldReturn` Right "g :: IO ()\ng = do\n  putStrLn \"a\"\n  printAll\n\nprintAll :: IO ()\nprintAll =\n  mapM_ print\n    [1, 2, 3 :: Int]\n"
    it "moves the later lines of a selection by the change in its first column" $ do
      let src = "k :: Int\nk = sum [ 1\n        , 2 ]\n"
      fs <- facts (2, 9) (3, 13) (2, 1) (3, 13) [] CallIsPart
      applied src fs Nothing "xs" `shouldReturn` Right "k :: Int\nk = sum xs\n\nxs =\n  [ 1\n  , 2 ]\n"
    it "passes a variable once however often the selection uses it" $ do
      let src = "h x = x + x\n"
      fs <- facts (1, 7) (1, 12) (1, 1) (1, 12) ["x", "x"] CallIsWhole
      applied src fs Nothing "dbl" `shouldReturn` Right "h x = dbl x\n\ndbl x =\n  x + x\n"
    it "passes the variables in the order given" $ do
      let src = "h x y = x + y\n"
      fs <- facts (1, 9) (1, 14) (1, 1) (1, 14) ["y", "x"] CallIsWhole
      applied src fs Nothing "add" `shouldReturn` Right "h x y = add y x\n\nadd y x =\n  x + y\n"
    it "keeps blank lines of the selection as they are" $ do
      let src = "m = do\n  a\n\n  b\n"
      fs <- facts (2, 3) (4, 4) (1, 1) (4, 4) [] CallIsWhole
      applied src fs Nothing "two" `shouldReturn` Right "m = do\n  two\n\ntwo =\n  a\n\n  b\n"
    it "refuses a free variable that is not a variable name" $ do
      let src = "x = 1\n"
      fs <- facts (1, 5) (1, 6) (1, 1) (1, 6) ["+."] CallIsWhole
      (refusal <$> applied src fs Nothing "a") `shouldReturn` Just (FreeVariableNotAVariable "+.")
    it "refuses a new name that a free variable already has" $ do
      let src = "h x = x + 1\n"
      fs <- facts (1, 7) (1, 12) (1, 1) (1, 12) ["x"] CallIsWhole
      (refusal <$> applied src fs Nothing "x") `shouldReturn` Just (NameClashesWithVariable "x")
    it "refuses a selection outside its declaration" $ do
      let src = "a = 1\nb = 2\n"
      fs <- facts (2, 5) (2, 6) (1, 1) (1, 6) [] CallIsWhole
      (refusal <$> applied src fs Nothing "c") `shouldReturn` Just SelectionNotInDeclaration
    it "refuses an empty selection" $ do
      let src = "a = 1\n"
      fs <- facts (1, 5) (1, 5) (1, 1) (1, 6) [] CallIsWhole
      (refusal <$> applied src fs Nothing "c") `shouldReturn` Just EmptySelection
    it "refuses a selection the text does not have" $ do
      let src = "a = 1\n"
      fs <- facts (1, 5) (1, 40) (1, 1) (1, 50) [] CallIsWhole
      (refusal <$> applied src fs Nothing "c") `shouldReturn` Just SelectionNotInText
    it "refuses a later line indented with a tab" $ do
      let src = "a = f\n\tx\n"
      fs <- facts (1, 5) (2, 10) (1, 1) (2, 10) [] CallIsWhole
      (refusal <$> applied src fs Nothing "c") `shouldReturn` Just SelectionHasTabIndent
    it "refuses a later line that would be left of the top-level indentation" $ do
      let src = "a = foo\n bar\n"
      fs <- facts (1, 5) (2, 5) (1, 1) (2, 5) [] CallIsWhole
      (refusal <$> applied src fs Nothing "c") `shouldReturn` Just SelectionLeavesLayout
    it "says what it refused in words" $
      renderError (RefactorFailure SelectionLeavesLayout) `shouldSatisfy` T.isInfixOf "indented too little"
