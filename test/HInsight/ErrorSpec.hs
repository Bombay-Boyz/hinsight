module HInsight.ErrorSpec (spec) where

import Data.Text qualified as T
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
  it "names the file and what GHC said when a dependency failed" $ do
    let t = rendered (SessionFailure (DependencyFailed "a.hs" "Bad.hs:3:1: error"))
    t `shouldSatisfy` T.isInfixOf "a.hs"
    t `shouldSatisfy` T.isInfixOf "Bad.hs:3:1: error"
  it "names the file that is missing from the module graph" $
    rendered (SessionFailure (TargetNotInGraph "a.hs")) `shouldSatisfy` T.isInfixOf "a.hs"
  it "names the file and the cause of an invalid value from GHC" $ do
    let t = rendered (ExtractionFailure (InvalidFromGhc "a.hs" (NonPositiveLine 0)))
    t `shouldSatisfy` T.isInfixOf "a.hs"
    t `shouldSatisfy` T.isInfixOf "line number 0"
  it "names both ends of a span that ends before it starts" $
    rendered (DomainFailure (SpanEndsBeforeStart 3 7 3 6)) `shouldSatisfy` T.isInfixOf "3:7"
  it "names how many stages a pipeline was given" $
    rendered (DomainFailure (TooFewStages 1)) `shouldSatisfy` T.isInfixOf "1"
  it "names the module that is listed twice" $
    rendered (DomainFailure (DuplicateModule "App")) `shouldSatisfy` T.isInfixOf "App"
  it "names both modules of an unknown import" $ do
    let t = rendered (DomainFailure (UnknownImport "App" "Missing"))
    t `shouldSatisfy` T.isInfixOf "App"
    t `shouldSatisfy` T.isInfixOf "Missing"
  it "names the root that is not absolute" $
    rendered (DomainFailure (RelativeRoot "pkg/sub")) `shouldSatisfy` T.isInfixOf "pkg/sub"
  it "names the flag that is unusable" $
    rendered (DomainFailure (InvalidFlag "-DX\NUL")) `shouldSatisfy` T.isInfixOf "-DX"
  it "names the version that is malformed" $
    rendered (DomainFailure (MalformedVersion "9..3")) `shouldSatisfy` T.isInfixOf "9..3"
  it "names both compilers of a mismatch and what to do" $ do
    let t = renderEnvironmentError (CompilerMismatch "9.14.1" "9.10.3")
    t `shouldSatisfy` T.isInfixOf "9.14.1"
    t `shouldSatisfy` T.isInfixOf "ghc-9.10.3"
  it "names the flag GHC refused, through the top-level renderer" $
    rendered (EnvironmentFailure (FlagRejected "unknown flag: -fnope")) `shouldSatisfy` T.isInfixOf "-fnope"
  it "names every argument GHC did not take for a flag" $ do
    let t = rendered (EnvironmentFailure (UnrecognisedArguments ["Foo.hs", "bar"]))
    t `shouldSatisfy` T.isInfixOf "Foo.hs"
    t `shouldSatisfy` T.isInfixOf "bar"
  prop "always names the path it was given" $ \p ->
    T.pack p `T.isInfixOf` rendered (SessionFailure (SourceFileNotFound p))
  prop "always names the offending number" $ \n ->
    T.pack (show n) `T.isInfixOf` rendered (DomainFailure (NonPositiveColumn n))
