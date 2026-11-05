-- |
-- Module      : HInsight.RefactorGhcSpec
-- Description : GHC's renamed program supplies the facts of a selection.
module HInsight.RefactorGhcSpec (spec) where

import Data.Text (Text)
import HInsight.Config (mkLibDir)
import HInsight.Error (InsightError (..), RefactorError (..))
import HInsight.Ghc.Session (ghcExtractFacts)
import HInsight.Identifier (mkIdentifier)
import HInsight.Refactor (CallContext (..), ExtractFacts (..))
import HInsight.Source (Position, Span, mkColumn, mkLine, mkPosition, mkSourceFile, mkSpan)
import HInsight.Support (ok)
import HInsight.Toolchain (findGhcLibDir)
import Test.Hspec

at :: Int -> Int -> IO Position
at l c = mkPosition <$> ok (mkLine l) <*> ok (mkColumn c)

spanOf :: (Int, Int) -> (Int, Int) -> IO Span
spanOf (l1, c1) (l2, c2) = do
  a <- at l1 c1
  b <- at l2 c2
  ok (mkSpan a b)

spec :: Spec
spec = describe "ghcExtractFacts, read from GHC" $ do
  libDir <- runIO findGhcLibDir
  maybe
    (it "reads the facts of a selection" (pendingWith "ghc-9.10.3 was not found; install it or set HINSIGHT_GHC_LIBDIR"))
    realSpec
    libDir

factsOf :: FilePath -> FilePath -> (Int, Int) -> (Int, Int) -> IO (Either InsightError ExtractFacts)
factsOf libDir path from to = do
  dir <- ok (mkLibDir libDir)
  file <- ok (mkSourceFile path)
  selection <- spanOf from to
  ghcExtractFacts dir file selection

fixture :: FilePath
fixture = "test/fixtures/refactor/Extract.hs"

expected :: FilePath -> (Int, Int) -> (Int, Int) -> (Int, Int) -> (Int, Int) -> [Text] -> CallContext -> IO ExtractFacts
expected path from to declFrom declTo vars ctx = do
  file <- ok (mkSourceFile path)
  selection <- spanOf from to
  declaration <- spanOf declFrom declTo
  names <- traverse (ok . mkIdentifier) vars
  pure (ExtractFacts file selection declaration names ctx)

realSpec :: FilePath -> Spec
realSpec libDir = do
  it "reads a variable bound outside the selection, and a let right-hand side as a whole" $ do
    want <- expected fixture (4, 17) (4, 22) (4, 1) (4, 43) ["n"] CallIsWhole
    factsOf libDir fixture (4, 17) (4, 22) `shouldReturn` Right want
  it "reads the variables of a parenthesised operand in the order they are used" $ do
    want <- expected fixture (4, 26) (4, 33) (4, 1) (4, 43) ["m", "k"] CallIsPart
    factsOf libDir fixture (4, 26) (4, 33) `shouldReturn` Right want
  it "reads an expression inside parentheses as a whole" $ do
    want <- expected fixture (4, 27) (4, 32) (4, 1) (4, 43) ["m", "k"] CallIsWhole
    factsOf libDir fixture (4, 27) (4, 32) `shouldReturn` Right want
  it "reads a whole right-hand side" $ do
    want <- expected fixture (7, 7) (7, 12) (7, 1) (7, 12) ["x"] CallIsWhole
    factsOf libDir fixture (7, 7) (7, 12) `shouldReturn` Right want
  it "does not count a variable bound inside the selection" $ do
    want <- expected fixture (4, 9) (4, 43) (4, 1) (4, 43) ["n", "k"] CallIsWhole
    factsOf libDir fixture (4, 9) (4, 43) `shouldReturn` Right want
  it "refuses a selection that is not exactly one expression" $
    factsOf libDir fixture (4, 17) (4, 20) `shouldReturn` Left (RefactorFailure SelectionNotAnExpression)
  it "refuses a selection outside every top-level binding" $
    factsOf libDir fixture (3, 6) (3, 9) `shouldReturn` Left (RefactorFailure SelectionNotInDeclaration)
  it "refuses a file that does not typecheck" $
    factsOf libDir "test/fixtures/refactor/Broken.hs" (4, 5) (4, 11) `shouldReturn` Left (RefactorFailure (FileDoesNotCompile 1))
