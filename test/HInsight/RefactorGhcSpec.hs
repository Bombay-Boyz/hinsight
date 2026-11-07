-- |
-- Module      : HInsight.RefactorGhcSpec
-- Description : GHC's renamed program supplies the facts of a selection.
module HInsight.RefactorGhcSpec (spec) where

import Control.Exception (finally)
import Data.Either (isLeft)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import HInsight (Refactor (..), analyseFile, analysisHoles, analysisMismatches, analysisUnexplained, mkLibDir)
import HInsight.Edit (applyEditSet)
import HInsight.Error (InsightError (..), RefactorError (..))
import HInsight.Ghc.Session (ghcExtractFacts, ghcInsight, ghcRefactor)
import HInsight.Identifier (mkIdentifier)
import HInsight.Refactor (CallContext (..), ExtractFacts (..), mkFunctionName)
import HInsight.Refactor.Verify (VerifiedPlan, planEdits, planNotes)
import HInsight.Source (Position, Span, mkColumn, mkLine, mkPosition, mkSourceFile, mkSpan)
import HInsight.Support (ok)
import HInsight.Toolchain (findGhcLibDir)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removeDirectoryRecursive)
import System.FilePath (takeFileName, (</>))
import System.Process (getCurrentPid)
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
    (\dir -> realSpec dir >> realExtractSpec dir)
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

planned :: FilePath -> (Int, Int) -> (Int, Int) -> Text -> IO (Either InsightError VerifiedPlan)
planned libDir from to name = do
  dir <- ok (mkLibDir libDir)
  file <- ok (mkSourceFile fixture)
  selection <- spanOf from to
  function <- either (fail . show) pure (mkFunctionName name)
  extractFunction (ghcRefactor dir) file selection function

applyPlan :: VerifiedPlan -> IO Text
applyPlan plan = do
  file <- ok (mkSourceFile fixture)
  source <- TIO.readFile fixture
  texts <- either (fail . show) pure (applyEditSet (const (Just source)) (planEdits plan))
  maybe (fail "the plan gave no text") pure (M.lookup file texts)

realExtractSpec :: FilePath -> Spec
realExtractSpec libDir = describe "extractFunction of ghcRefactor" $ do
  it "extracts an expression with the signature GHC gave" $ do
    Right plan <- planned libDir (4, 17) (4, 22) "twice"
    planNotes plan `shouldBe` []
    applyPlan plan
      `shouldReturn` "module Extract (f, g) where\n\nf :: Int -> Int -> Int\nf k n = let m = twice n in (m + k) * (m + k)\n\ntwice :: forall {a}. Num a => a -> a\ntwice n =\n  n * 2\n\ng :: Int -> Int\ng x = x + 1\n"
  it "passes the variables the selection uses, in the order it uses them" $ do
    Right plan <- planned libDir (4, 26) (4, 33) "sq"
    planNotes plan `shouldBe` []
    applyPlan plan
      `shouldReturn` "module Extract (f, g) where\n\nf :: Int -> Int -> Int\nf k n = let m = n * 2 in (sq m k) * (m + k)\n\nsq :: forall {a}. Num a => a -> a -> a\nsq m k =\n  (m + k)\n\ng :: Int -> Int\ng x = x + 1\n"
  it "refuses a name the module already defines, with GHC's message" $ do
    result <- planned libDir (4, 17) (4, 22) "g"
    result `shouldSatisfy` isLeft
    result `shouldSatisfy` either (maybe False (not . T.null) . rejection) (const False)
  it "leaves a file that GHC analyses with no error" $ do
    Right plan <- planned libDir (4, 17) (4, 22) "twice"
    changed <- applyPlan plan
    temporary <- getTemporaryDirectory
    pid <- getCurrentPid
    let folder = temporary </> ("hinsight-extract-" <> show pid)
        target = folder </> takeFileName fixture
    createDirectoryIfMissing True folder
    ( do
        TIO.writeFile target changed
        dir <- ok (mkLibDir libDir)
        file <- ok (mkSourceFile target)
        analysis <- analyseFile (ghcInsight dir) file >>= either (fail . show) pure
        (analysisHoles analysis, analysisMismatches analysis, analysisUnexplained analysis) `shouldBe` ([], [], 0)
      )
      `finally` removeDirectoryRecursive folder

rejection :: InsightError -> Maybe Text
rejection = \case
  RefactorFailure (VerificationFailed why) -> Just why
  _ -> Nothing
