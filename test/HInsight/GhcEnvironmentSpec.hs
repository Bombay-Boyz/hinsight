-- |
-- Module      : HInsight.GhcEnvironmentSpec
-- Description : Tests that run a real GHC inside a project environment.
--
-- Each check is the named regression test of a failure class in
-- @docs/prior-art.md@ ("the project environment"). They use the same GHC as
-- "HInsight.GhcSessionSpec" and are reported as pending when none is found.
module HInsight.GhcEnvironmentSpec (spec) where

import Data.Text (Text)
import Data.Text qualified as T
import HInsight
import HInsight.Environment
  ( Environment,
    mkComponentName,
    mkEnvironment,
    mkGhcFlag,
    mkProjectRoot,
    pinnedGhcVersion,
  )
import HInsight.Ghc.Session (ghcInsightIn)
import HInsight.Support (ok)
import HInsight.Toolchain (findGhcLibDir)
import HInsight.TypeText (unTypeText)
import System.Directory (canonicalizePath)
import Test.Hspec

-- | An environment rooted at this folder, with these flags.
environmentIn :: FilePath -> [Text] -> IO Environment
environmentIn dir flags = do
  root <- canonicalizePath dir >>= ok . mkProjectRoot
  component <- ok (mkComponentName "fixture")
  parsed <- traverse (ok . mkGhcFlag) flags
  ok (mkEnvironment component root pinnedGhcVersion parsed)

analyseIn :: FilePath -> Environment -> FilePath -> IO (Either InsightError Analysis)
analyseIn libDir env file = do
  dir <- ok (mkLibDir libDir)
  source <- ok (mkSourceFile file)
  analyseFile (ghcInsightIn dir env) source

succeeded :: Either InsightError Analysis -> IO Analysis
succeeded = either (fail . T.unpack . renderError) pure

-- | What the file's errors came to: explained mismatches and unexplained ones.
counts :: Analysis -> (Int, Int)
counts a = (length (analysisMismatches a), analysisUnexplained a)

spec :: Spec
spec = describe "ghcInsightIn" $ do
  libDir <- runIO findGhcLibDir
  maybe
    (it "analyses in an environment" (pendingWith "ghc-9.10.3 was not found; install it or set HINSIGHT_GHC_LIBDIR"))
    environmentSpec
    libDir

environmentSpec :: FilePath -> Spec
environmentSpec libDir = do
  it "applies a default extension of the project" $ do
    let file = "test/fixtures/env/ext/NeedsExt.hs"
    without <- environmentIn "test/fixtures/env/ext" ["-i."] >>= \env -> analyseIn libDir env file >>= succeeded
    with <- environmentIn "test/fixtures/env/ext" ["-i.", "-XLambdaCase"] >>= \env -> analyseIn libDir env file >>= succeeded
    -- Without the extension the file does not parse, which GHC reports but
    -- hinsight cannot explain; with it the type mismatch is explained.
    counts without `shouldBe` (0, 1)
    case analysisMismatches with of
      [m] -> unTypeText (explanationActual m) `shouldBe` "Int"
      other -> expectationFailure ("expected exactly one mismatch, found " <> show (length other))
  it "applies a preprocessor definition of the project" $ do
    let file = "test/fixtures/env/cpp/Cpp.hs"
    plain <- environmentIn "test/fixtures/env/cpp" ["-i."] >>= \env -> analyseIn libDir env file >>= succeeded
    defined <- environmentIn "test/fixtures/env/cpp" ["-i.", "-DBROKEN"] >>= \env -> analyseIn libDir env file >>= succeeded
    counts plain `shouldBe` (0, 0)
    counts defined `shouldBe` (1, 0)
  it "reports a flag GHC refuses as a typed error" $ do
    env <- environmentIn "test/fixtures/env/ext" ["-i.", "-fnot-a-real-flag"]
    result <- analyseIn libDir env "test/fixtures/env/ext/NeedsExt.hs"
    result `shouldSatisfy` \case
      Left (EnvironmentFailure (FlagRejected why)) -> "not-a-real-flag" `T.isInfixOf` why
      _ -> False
  it "reports an argument GHC takes for a file name as a typed error" $ do
    env <- environmentIn "test/fixtures/env/ext" ["-i.", "Elsewhere.hs"]
    result <- analyseIn libDir env "test/fixtures/env/ext/NeedsExt.hs"
    result `shouldBe` Left (EnvironmentFailure (UnrecognisedArguments ["Elsewhere.hs"]))
  it "projectWerrorKeepsErrorCount: a project -Werror changes no count" $ do
    let file = "test/fixtures/env/werror/Werror.hs"
    plain <- environmentIn "test/fixtures/env/werror" ["-i."] >>= \env -> analyseIn libDir env file >>= succeeded
    strict <- environmentIn "test/fixtures/env/werror" ["-i.", "-Wall", "-Werror"] >>= \env -> analyseIn libDir env file >>= succeeded
    counts plain `shouldBe` (1, 0)
    counts strict `shouldBe` counts plain
  it "templateHaskellInProjectModule: analyses a module that splices a project function" $ do
    env <- environmentIn "test/fixtures/env/th" ["-i."]
    a <- analyseIn libDir env "test/fixtures/env/th/UsesTh.hs" >>= succeeded
    -- The splice yields a String where an Int is declared.
    case analysisMismatches a of
      [m] -> unTypeText (explanationExpected m) `shouldBe` "Int"
      other -> expectationFailure ("expected exactly one mismatch, found " <> show (length other))
  it "dogfood: analyses one of its own modules with its own flags" $ do
    env <-
      environmentIn
        "."
        ["-isrc", "-XGHC2021", "-XLambdaCase", "-XOverloadedStrings", "-XDerivingStrategies", "-Wall", "-Werror"]
    a <- analyseIn libDir env "src/HInsight/Environment.hs" >>= succeeded
    counts a `shouldBe` (0, 0)
    map unModuleName (graphModules (analysisGraph a)) `shouldContain` ["HInsight.Environment", "HInsight.Error"]
