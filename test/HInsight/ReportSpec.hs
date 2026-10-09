module HInsight.ReportSpec (spec) where

import Data.Text qualified as T
import HInsight.Analysis (Analysis (..), emptyAnalysis)
import HInsight.Explanation (mkTypeText)
import HInsight.Hole
import HInsight.Identifier (mkIdentifier)
import HInsight.Report
import HInsight.Source
import HInsight.Support (genAnalysis, genHoleReport, ok)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

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
        `shouldBe` "nothing to report: no type mismatches or typed holes found\n"
    it "reports a single unexplained error in the singular" $
      renderAnalysis emptyAnalysis {analysisUnexplained = 1}
        `shouldBe` "1 other error not explained by this version\n"
    it "reports several unexplained errors in the plural" $
      renderAnalysis emptyAnalysis {analysisUnexplained = 3}
        `shouldBe` "3 other errors not explained by this version\n"
    it "separates sections with a blank line" $ do
      h <- holeAt []
      let out = renderAnalysis emptyAnalysis {analysisHoles = [h, h]}
      length (filter T.null (T.lines out)) `shouldBe` 1
    prop "never produces empty output" $
      forAll genAnalysis $
        \a -> not (T.null (renderAnalysis a))
    prop "mentions every hole it was given" $
      forAll genAnalysis $ \a ->
        length (T.breakOnAll "typed hole " (renderAnalysis a)) === length (analysisHoles a)
