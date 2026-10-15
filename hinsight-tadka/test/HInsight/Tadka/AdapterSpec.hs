module HInsight.Tadka.AdapterSpec (spec) where

import Control.Monad (void)
import Data.Bifunctor (first)
import Data.Maybe (isJust)
import Data.Text qualified as T
import HInsight.Analysis (emptyAnalysis)
import HInsight.Analysis qualified as A
import HInsight.Context (Context (..))
import HInsight.Environment (mkComponentName, mkEnvironment, mkGhcFlag, mkProjectRoot, pinnedGhcVersion, summarise)
import HInsight.Flow (FlowKind (..))
import HInsight.Hole (Locality (..))
import HInsight.Tadka
import HInsight.Tadka.Support
import Tadka qualified as Td
import Test.Hspec

-- | One finding of each kind, over a source made for it.
mismatchFinding :: IO Finding
mismatchFinding = do
  src <- sourceOf "answer :: String\nanswer = length \"hi\"\n"
  sig <- span' (1, 1) (1, 17)
  sp <- span' (2, 10) (2, 21)
  ctx <- signedAt "answer" sig "String"
  e <- mismatchAt sp "String" "Int" (Just ("Int", "[Char]")) ctx
  oneOf src (analysisOf [e] [] [])

oneOf :: Td.NamedSource -> A.Analysis -> IO Finding
oneOf src a =
  ok (findings src a) >>= \case
    [f] -> pure f
    fs -> fail ("expected one finding, got " <> show (length fs))

holeFinding :: IO Finding
holeFinding = do
  src <- sourceOf "double :: Int -> Int\ndouble x = _ + x\n"
  sp <- span' (2, 12) (2, 13)
  h <- holeAt sp "Int" [("x", "Int", 0, Local), ("maxBound", "forall a. Bounded a => a", 0, Imported)]
  oneOf src (analysisOf [] [h] [])

pipelineFinding :: IO Finding
pipelineFinding = do
  src <- sourceOf "f = show . length\n"
  sp <- span' (1, 5) (1, 18)
  p <- pipelineAt sp Composition [("show", "Int -> String"), ("length", "[a] -> Int")] "[a] -> String" OutsideBinding
  oneOf src (analysisOf [] [] [p])

spec :: Spec
spec = describe "findings" $ do
  it "draws nothing for an analysis that found nothing" $ do
    src <- sourceOf "x = 1\n"
    fmap length (ok (findings src emptyAnalysis)) `shouldReturn` 0
  it "gives every kind a code that Tadka accepts, and a different one each" $ do
    codes <- traverse (fmap Td.code) [mismatchFinding, holeFinding, pipelineFinding]
    all isJust codes `shouldBe` True
    length (uniq (map (fmap Td.unDiagnosticCode) codes)) `shouldBe` 3
  it "reports a mismatch and a hole as errors and a pipeline as advice" $ do
    sevs <- traverse (fmap Td.severity) [mismatchFinding, holeFinding, pipelineFinding]
    sevs `shouldBe` [Td.SevError, Td.SevError, Td.SevAdvice]
  it "marks the expression and the signature of a signed binding" $ do
    f <- mismatchFinding
    length (Td.contextLabelStates (Td.context f)) `shouldBe` 2
    all isResolved (Td.contextLabelStates (Td.context f)) `shouldBe` True
  it "says what GHC expected, what it found and what it could not match" $ do
    t <- narrate <$> mismatchFinding
    t `shouldSatisfy` T.isInfixOf "expected \"String\", found \"Int\""
    t `shouldSatisfy` T.isInfixOf "could not match \"Int\" with \"[Char]\""
    t `shouldSatisfy` T.isInfixOf "\"answer\" is declared as \"String\""
  it "points an unsigned binding at its missing signature instead of a secondary label" $ do
    src <- sourceOf "flag = not 'x'\n"
    sp <- span' (1, 12) (1, 15)
    name <- ident "flag"
    e <- mismatchAt sp "Bool" "Char" Nothing (InUnsignedBinding name)
    f <- oneOf src (analysisOf [e] [] [])
    length (Td.contextLabelStates (Td.context f)) `shouldBe` 1
    narrate f `shouldSatisfy` T.isInfixOf "has no type signature"
  it "reads aloud as Tadka 2.0's narratable renderer words a mismatch" $ do
    t <- narrate <$> mismatchFinding
    T.lines t
      `shouldBe` [ "Error, code hinsight::E0001: type mismatch: expected \"String\", found \"Int\".",
                   "Location: Sample.hs, line 2, column 10.",
                   "Source line 2: \"answer = length \"hi\"\".",
                   "The problem is at columns 10 through 20, labeled: expected \"String\", but this has type \"Int\".",
                   "Source line 1: \"answer :: String\".",
                   "Related context is at columns 1 through 16, labeled: \"answer\" is declared as \"String\".",
                   "Help: GHC could not match \"Int\" with \"[Char]\""
                 ]
  it "lists hole fits in the order hinsight ranked them" $ do
    t <- narrate <$> holeFinding
    t `shouldSatisfy` T.isInfixOf "fits, best first"
    t `shouldSatisfy` (\s -> indexOf "x :: Int" s < indexOf "maxBound ::" s)
  it "lists the stages of a pipeline in the order the data flows" $ do
    t <- narrate <$> pipelineFinding
    t `shouldSatisfy` (\s -> indexOf "show" s < indexOf "length" s)
    t `shouldSatisfy` T.isInfixOf "as a whole: \"[a] -> String\""
  it "renders as JSON" $ do
    fs <- sequence [mismatchFinding, holeFinding, pipelineFinding]
    map describeJson fs `shouldBe` [True, True, True]
  it "counts a tab as GHC does, so a label lands on the right characters" $ do
    src <- sourceOf "\tfoo bar\n"
    sp <- span' (1, 9) (1, 12)
    e <- mismatchAt sp "Int" "Bool" Nothing OutsideBinding
    f <- oneOf src (analysisOf [e] [] [])
    case Td.contextLabelStates (Td.context f) of
      [Td.LabelOk r] -> (Td.resolvedStart r, Td.resolvedEnd r) `shouldBe` (Td.LineCol 1 2, Td.LineCol 1 5)
      other -> expectationFailure ("unexpected labels: " <> show other)
  it "refuses a span that is not in the text, naming it" $ do
    src <- sourceOf "x = 1\n"
    sp <- span' (9, 1) (9, 4)
    e <- mismatchAt sp "Int" "Bool" Nothing OutsideBinding
    fmap length (findings src (analysisOf [e] [] [])) `shouldBe` Left (UnusableSpan 9 1 9 4)
  it "refuses an empty source name" $
    void (namedSource "" "x") `shouldBe` Left UnnamedSource
  describe "renderExtras" $ do
    it "is empty when there is nothing beyond the diagnostics" $
      renderExtras emptyAnalysis `shouldBe` ""
    it "counts errors it did not explain, in the singular and the plural" $ do
      renderExtras emptyAnalysis {A.analysisUnexplained = 1} `shouldBe` "1 other error not explained by this version\n"
      renderExtras emptyAnalysis {A.analysisUnexplained = 2} `shouldBe` "2 other errors not explained by this version\n"
    it "names the environment the analysis ran in" $ do
      summary <- either (fail . show) pure $ do
        component <- first show (mkComponentName "lib:one")
        root <- first show (mkProjectRoot "/work/one")
        flag <- first show (mkGhcFlag "-Werror")
        env <- first show (mkEnvironment component root pinnedGhcVersion [flag])
        pure (summarise env)
      renderExtras emptyAnalysis {A.analysisEnvironment = Just summary}
        `shouldBe` "environment: component lib:one, 1 flag; left out of the session: -Werror\n"
  where
    isResolved :: Td.LabelState -> Bool
    isResolved = \case
      Td.LabelOk _ -> True
      Td.LabelStale _ -> False
    uniq :: (Eq a) => [a] -> [a]
    uniq = foldr (\x acc -> if x `elem` acc then acc else x : acc) []
    -- Where a phrase first appears; the text length if it does not appear.
    indexOf :: T.Text -> T.Text -> Int
    indexOf needle = T.length . fst . T.breakOn needle
