-- |
-- Module      : HInsight.Cabal.CaptureSpec
-- Description : Tests of the pure step from cabal's report to an environment.
module HInsight.Cabal.CaptureSpec (spec) where

import Control.Monad (void)
import Data.Text (Text)
import Data.Text qualified as T
import HInsight.Cabal.Capture (Captured (..), componentNameOf, renderCaptureFailure, toEnvironment)
import HInsight.Environment (environmentComponent, environmentFlags, environmentRoot, projectRootPath, unComponentName, unGhcFlag)
import HInsight.Error (DomainError (..), EnvironmentError (..), InsightError (..))
import System.Exit (ExitCode (..))
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (elements, forAll, listOf, (===))

captured :: [String] -> Captured
captured options = Captured {capturedRoot = "/work/one", capturedOptions = options, capturedVersion = "9.10.3"}

spec :: Spec
spec = do
  describe "toEnvironment" $ do
    it "keeps the root, the flags and their order, and names the component by its unit id" $
      case toEnvironment (captured ["-XLambdaCase", "-this-unit-id", "one-0.1-inplace", "-isrc"]) of
        Right env -> do
          projectRootPath (environmentRoot env) `shouldBe` "/work/one"
          map unGhcFlag (environmentFlags env) `shouldBe` ["-XLambdaCase", "-this-unit-id", "one-0.1-inplace", "-isrc"]
          unComponentName (environmentComponent env) `shouldBe` "one-0.1-inplace"
        Left err -> expectationFailure (show err)
    it "otherCompilerIsRefused: reports both compilers for a plan built by another GHC" $
      void (toEnvironment (captured []) {capturedVersion = "9.14.1"})
        `shouldBe` Left (EnvironmentFailure (CompilerMismatch "9.14.1" "9.10.3"))
    it "refuses a root that is not absolute" $
      void (toEnvironment (captured []) {capturedRoot = "one"})
        `shouldBe` Left (DomainFailure (RelativeRoot "one"))
    it "refuses a compiler version it cannot read" $
      void (toEnvironment (captured []) {capturedVersion = "ghc"})
        `shouldBe` Left (DomainFailure (MalformedVersion "ghc"))
    it "drops empty arguments, which are not flags" $
      fmap (map unGhcFlag . environmentFlags) (toEnvironment (captured ["-O", "", "-Wall"]))
        `shouldBe` Right ["-O", "-Wall"]
    it "names a component that cabal gave no unit id as unnamed" $
      fmap (unComponentName . environmentComponent) (toEnvironment (captured ["-O"])) `shouldBe` Right "unnamed"
    prop "never loses or reorders a non-empty argument" $
      forAll (listOf (elements ["-O", "-Wall", "-XLambdaCase", "-isrc", "-package-id", "base-4.20", ""])) $ \options ->
        fmap (map unGhcFlag . environmentFlags) (toEnvironment (captured options))
          === Right (map T.pack (filter (not . null) options))
  describe "componentNameOf" $ do
    it "reads the unit id written as a separate argument" $
      componentNameOf ["-O", "-this-unit-id", "a-1-inplace"] `shouldBe` Just "a-1-inplace"
    it "reads the unit id written with an equals sign" $
      componentNameOf ["-this-unit-id=a-1-inplace"] `shouldBe` Just "a-1-inplace"
    it "takes the last one, as GHC does" $
      componentNameOf ["-this-unit-id", "first", "-this-unit-id=second", "-this-unit-id", "third"] `shouldBe` Just "third"
    it "is nothing when there is none, or the flag has no value" $ do
      componentNameOf ["-O"] `shouldBe` Nothing
      componentNameOf ["-O", "-this-unit-id"] `shouldBe` Nothing
    it "finds the flag straight after another flag" $
      componentNameOf ["-package-id", "-this-unit-id", "x"] `shouldBe` Just "x"
  describe "renderCaptureFailure" $ do
    it "gives the exit status and what cabal wrote, on one line" $ do
      let t = renderCaptureFailure (ExitFailure 1) ["cabal: no such component", "  try again"]
      t `shouldSatisfy` T.isInfixOf "exit status 1"
      t `shouldSatisfy` T.isInfixOf "no such component"
      T.lines t `shouldSatisfy` ((== 1) . length)
    it "says so when cabal succeeded but gave no flags" $
      renderCaptureFailure ExitSuccess [] `shouldSatisfy` T.isInfixOf ("no flags" :: Text)
