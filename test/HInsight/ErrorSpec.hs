module HInsight.ErrorSpec (spec) where

import qualified Data.Text as T
import HInsight.Error
import Test.Hspec
import Test.Hspec.QuickCheck (prop)

rendered :: InsightError -> T.Text
rendered = renderError

spec :: Spec
spec = describe "renderError" $ do
  it "names the library directory that is missing" $
    rendered (SessionFailure (LibDirNotFound "/opt/ghc/lib")) `shouldSatisfy` T.isInfixOf "/opt/ghc/lib"
  it "names the file that is missing" $
    rendered (SessionFailure (SourceFileNotFound "src/Main.hs")) `shouldSatisfy` T.isInfixOf "src/Main.hs"
  it "names the file and the exception when GHC throws" $ do
    let t = rendered (SessionFailure (GhcThrew "a.hs" "panic"))
    t `shouldSatisfy` T.isInfixOf "a.hs"
    t `shouldSatisfy` T.isInfixOf "panic"
  it "names the file and the count for an unexpected module count" $ do
    let t = rendered (SessionFailure (UnexpectedModuleCount "a.hs" 3))
    t `shouldSatisfy` T.isInfixOf "a.hs"
    t `shouldSatisfy` T.isInfixOf "3"
  it "names the file and the cause of an invalid value from GHC" $ do
    let t = rendered (ExtractionFailure (InvalidFromGhc "a.hs" (NonPositiveLine 0)))
    t `shouldSatisfy` T.isInfixOf "a.hs"
    t `shouldSatisfy` T.isInfixOf "line number 0"
  it "names both ends of a span that ends before it starts" $
    rendered (DomainFailure (SpanEndsBeforeStart 3 7 3 6)) `shouldSatisfy` T.isInfixOf "3:7"
  prop "always names the path it was given" $ \p ->
    T.pack p `T.isInfixOf` rendered (SessionFailure (SourceFileNotFound p))
  prop "always names the offending number" $ \n ->
    T.pack (show n) `T.isInfixOf` rendered (DomainFailure (NonPositiveColumn n))
