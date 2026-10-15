module HInsight.ReportSpec (spec) where

import Data.Text qualified as T
import HInsight.Analysis (Analysis (..), emptyAnalysis)
import HInsight.Environment (EnvironmentSummary, mkComponentName, mkEnvironment, mkGhcFlag, mkProjectRoot, pinnedGhcVersion, summarise)
import HInsight.Graph (mkModuleName, mkProjectGraph)
import HInsight.Hole
import HInsight.Identifier (mkIdentifier)
import HInsight.Report
import HInsight.Source
import HInsight.Support (genAnalysis, genHoleReport, ok)
import HInsight.TypeText (mkTypeText)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

environmentSummary :: [T.Text] -> IO EnvironmentSummary
environmentSummary flags = do
  component <- ok (mkComponentName "lib:sample")
  root <- ok (mkProjectRoot "/work/sample")
  parsed <- traverse (ok . mkGhcFlag) flags
  summarise <$> ok (mkEnvironment component root pinnedGhcVersion parsed)

holeAt :: [HoleFit] -> IO HoleReport
holeAt fits = do
  s <- mkPosition <$> ok (mkLine 4) <*> ok (mkColumn 10)
  e <- mkPosition <$> ok (mkLine 4) <*> ok (mkColumn 11)
  sp <- ok (mkSpan s e)
  name <- ok (mkIdentifier "_")
  ty <- ok (mkTypeText "Int")
  pure (HoleReport name sp ty fits)

fitNamed :: String -> Locality -> [String] -> IO HoleFit
fitNamed name loc matches = do
  n <- ok (mkIdentifier (T.pack name))
  t <- ok (mkTypeText "Int")
  r <- ok (mkRefinementLevel (length matches))
  ms <- traverse (ok . mkTypeText . T.pack) matches
  pure (HoleFit n t r loc ms)

spec :: Spec
spec = do
  describe "renderHoleReport" $ do
    it "says when there are no fits" $ do
      h <- holeAt []
      T.lines (renderHoleReport h)
        `shouldBe` ["typed hole _ at 4:10, wanted type: Int", "  no fits found"]
    it "lists fits in the order given, with locality" $ do
      a <- fitNamed "x" Local []
      b <- fitNamed "maxBound" Imported []
      h <- holeAt [a, b]
      T.lines (renderHoleReport h)
        `shouldBe` [ "typed hole _ at 4:10, wanted type: Int",
                     "  fits, best first:",
                     "    x :: Int  [local]",
                     "    maxBound :: Int  [imported]"
                   ]
    it "shows the types of the new holes a fit leaves" $ do
      a <- fitNamed "f" Local ["Bool", "Char"]
      h <- holeAt [a]
      T.lines (renderHoleReport h)
        `shouldContain` ["    f :: Int  [local, leaves new holes: Bool, Char]"]
    prop "has a header plus either one line or a heading and one line per fit" $
      forAll genHoleReport $ \h ->
        length (T.lines (renderHoleReport h))
          === (1 + if null (holeFits h) then 1 else 1 + length (holeFits h))

  describe "renderAnalysis" $ do
    it "says so when nothing was found" $
      renderAnalysis emptyAnalysis
        `shouldBe` "nothing to report: no type mismatches, typed holes or pipelines found\n"
    it "reports a single unexplained error in the singular" $
      renderAnalysis emptyAnalysis {analysisUnexplained = 1}
        `shouldBe` "1 other error not explained by this version\n"
    it "reports several unexplained errors in the plural" $
      renderAnalysis emptyAnalysis {analysisUnexplained = 3}
        `shouldBe` "3 other errors not explained by this version\n"
    it "stays silent about a module graph without imports" $ do
      a <- ok (mkModuleName "A")
      g <- ok (mkProjectGraph [(a, [])])
      renderAnalysis emptyAnalysis {analysisGraph = g}
        `shouldBe` "nothing to report: no type mismatches, typed holes or pipelines found\n"
    it "shows the module graph when a module imports another" $ do
      a <- ok (mkModuleName "A")
      b <- ok (mkModuleName "B")
      g <- ok (mkProjectGraph [(a, [b]), (b, [])])
      renderAnalysis emptyAnalysis {analysisGraph = g}
        `shouldBe` "module graph: 2 modules, 1 import\n  B\n  A  imports B\n"
    it "names the environment after the findings, with what was left out of the session" $ do
      s <- environmentSummary ["-XLambdaCase", "-Werror", "-fwrite-ide-info"]
      renderAnalysis emptyAnalysis {analysisEnvironment = Just s}
        `shouldBe` T.unlines
          [ "nothing to report: no type mismatches, typed holes or pipelines found",
            "",
            "environment: component lib:sample, 3 flags; left out of the session: -Werror, -fwrite-ide-info"
          ]
    it "uses the singular for one flag and says nothing about flags that were all kept" $ do
      s <- environmentSummary ["-XLambdaCase"]
      renderEnvironmentSummary s `shouldBe` "environment: component lib:sample, 1 flag\n"
    it "separates sections with a blank line" $ do
      h <- holeAt []
      let out = renderAnalysis emptyAnalysis {analysisHoles = [h, h]}
      length (filter T.null (T.lines out)) `shouldBe` 1
    prop "never produces empty output" $
      forAll genAnalysis $
        \a -> not (T.null (renderAnalysis a))
    prop "mentions every pipeline it was given" $
      forAll genAnalysis $ \a ->
        length (T.breakOnAll "type flow at " (renderAnalysis a)) === length (analysisFlows a)
    prop "mentions every hole it was given" $
      forAll genAnalysis $ \a ->
        length (T.breakOnAll "typed hole " (renderAnalysis a)) === length (analysisHoles a)
