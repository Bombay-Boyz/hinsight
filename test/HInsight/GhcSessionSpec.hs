-- |
-- Module      : HInsight.GhcSessionSpec
-- Description : Tests that run a real GHC.
--
-- The checks that need a GHC use the library directory in
-- @HINSIGHT_GHC_LIBDIR@ when it is set, and otherwise ask @ghc-9.10.3@, the
-- compiler this project is pinned to. If neither works they are reported as
-- pending rather than silently passing.
module HInsight.GhcSessionSpec (spec) where

import Control.Exception (IOException, try)
import Data.Maybe (isJust)
import Data.Text (Text)
import HInsight
import HInsight.Error (SessionError (..))
import HInsight.Ghc.Session (ghcInsight)
import HInsight.Hole (Locality (..), unRefinementLevel)
import HInsight.Source (positionLine, spanStart, unLine)
import HInsight.Support (ok)
import HInsight.TypeText (unTypeText)
import System.Environment (lookupEnv)
import System.Process (readProcess)
import Test.Hspec

analyseFixture :: FilePath -> FilePath -> IO (Either InsightError Analysis)
analyseFixture libDir name = do
  dir <- ok (mkLibDir libDir)
  file <- ok (mkSourceFile ("test/fixtures/" <> name))
  analyseFile (ghcInsight dir) file

succeeded :: Either InsightError Analysis -> IO Analysis
succeeded = either (fail . show) pure

-- | The library directory to analyse with: the environment variable if set,
-- otherwise what the pinned compiler reports.
findLibDir :: IO (Maybe FilePath)
findLibDir = lookupEnv "HINSIGHT_GHC_LIBDIR" >>= maybe fromPinnedGhc (pure . Just)
  where
    fromPinnedGhc :: IO (Maybe FilePath)
    fromPinnedGhc = do
      result <- try @IOException (readProcess "ghc-9.10.3" ["--print-libdir"] "")
      pure (either (const Nothing) (Just . takeWhile (/= '\n')) result)

spec :: Spec
spec = describe "ghcInsight" $ do
  preflightSpec
  libDir <- runIO findLibDir
  maybe
    (it "analyses real files" (pendingWith "ghc-9.10.3 was not found; install it or set HINSIGHT_GHC_LIBDIR"))
    realGhcSpec
    libDir

-- | A context for a binding with this name, declared at this line as this type.
isSignedAs :: Text -> Int -> Text -> Context -> Bool
isSignedAs name line declared = \case
  InSignedBinding n sp t ->
    unIdentifier n == name
      && unLine (positionLine (spanStart sp)) == line
      && unTypeText t == declared
  _ -> False

stageTexts :: Pipeline -> [Text]
stageTexts = map (unStageText . stageText) . pipelineStages

stageTypes :: Pipeline -> [Text]
stageTypes = map (unTypeText . stageType) . pipelineStages

-- | A context for a binding with this name and no signature.
isUnsigned :: Text -> Context -> Bool
isUnsigned name = \case
  InUnsignedBinding n -> unIdentifier n == name
  _ -> False

-- | Failures that are detected before GHC starts, so they need no GHC.
preflightSpec :: Spec
preflightSpec = do
  it "reports a library directory that does not exist" $ do
    r <- analyseFixture "/nonexistent/hinsight-libdir" "Ok.hs"
    r `shouldBe` Left (SessionFailure (LibDirNotFound "/nonexistent/hinsight-libdir"))
  it "reports a source file that does not exist" $ do
    r <- analyseFixture "." "DoesNotExist.hs"
    r `shouldBe` Left (SessionFailure (SourceFileNotFound "test/fixtures/DoesNotExist.hs"))

realGhcSpec :: FilePath -> Spec
realGhcSpec libDir = do
  it "finds nothing in a correct file" $ do
    a <- analyseFixture libDir "Ok.hs" >>= succeeded
    a `shouldBe` emptyAnalysis
  it "explains a type mismatch" $ do
    a <- analyseFixture libDir "TypeMismatch.hs" >>= succeeded
    case analysisMismatches a of
      [m] -> do
        unTypeText (explanationActual m) `shouldBe` "Int"
        unTypeText (explanationExpected m) `shouldSatisfy` (`elem` ["String", "[Char]"])
        unLine (positionLine (spanStart (explanationSpan m))) `shouldBe` 4
        explanationContext m `shouldSatisfy` isSignedAs "greeting" 3 "String"
        -- String expands to [Char], which is news, so the divergence is kept.
        explanationDivergence m `shouldSatisfy` isJust
      other -> expectationFailure ("expected exactly one mismatch, found " <> show (length other))
  it "reports a binding without a signature as unsigned" $ do
    a <- analyseFixture libDir "UnsignedMismatch.hs" >>= succeeded
    case analysisMismatches a of
      [m] -> do
        unTypeText (explanationExpected m) `shouldBe` "Bool"
        unTypeText (explanationActual m) `shouldBe` "Char"
        explanationContext m `shouldSatisfy` isUnsigned "flag"
        -- The unmatched pair is just expected and actual again: no divergence.
        explanationDivergence m `shouldBe` Nothing
      other -> expectationFailure ("expected exactly one mismatch, found " <> show (length other))
  it "reports ranked fits for a typed hole" $ do
    a <- analyseFixture libDir "Hole.hs" >>= succeeded
    case analysisHoles a of
      [h] -> do
        unIdentifier (holeName h) `shouldBe` "_"
        unTypeText (holeType h) `shouldBe` "Int"
        fmap (unIdentifier . fitName) (filter ((== Local) . fitLocality) (holeFits h)) `shouldContain` ["x"]
        fmap (unRefinementLevel . fitRefinement) (take 1 (holeFits h)) `shouldBe` [0]
      other -> expectationFailure ("expected exactly one hole, found " <> show (length other))
  it "counts an error it does not explain" $ do
    a <- analyseFixture libDir "ScopeError.hs" >>= succeeded
    (analysisMismatches a, analysisHoles a, analysisUnexplained a) `shouldBe` ([], [], 1)
  it "shows the type at each stage of each pipeline" $ do
    a <- analyseFixture libDir "Pipeline.hs" >>= succeeded
    let flows = analysisFlows a
    fmap (\p -> (unLine (positionLine (spanStart (pipelineSpan p))), pipelineKind p)) flows
      `shouldBe` [(4, Composition), (7, Application), (10, Bind), (10, Composition)]
    case flows of
      [composed, applied, bound, inner] -> do
        stageTexts composed `shouldBe` ["words", "map length", "sum"]
        take 1 (stageTypes composed) `shouldBe` ["String -> [String]"]
        drop 2 (stageTypes composed) `shouldBe` ["[Int] -> Int"]
        unTypeText (pipelineType composed) `shouldBe` "String -> Int"
        stageTexts applied `shouldBe` ["words s", "map reverse", "unwords"]
        take 1 (stageTypes applied) `shouldBe` ["[String]"]
        drop 2 (stageTypes applied) `shouldBe` ["[String] -> String"]
        unTypeText (pipelineType applied) `shouldBe` "String"
        stageTexts bound `shouldBe` ["getLine", "putStrLn . reverse"]
        take 1 (stageTypes bound) `shouldBe` ["IO String"]
        stageTexts inner `shouldBe` ["reverse", "putStrLn"]
        pipelineContext composed `shouldSatisfy` isSignedAs "countWords" 3 "String -> Int"
      other -> expectationFailure ("expected four pipelines, found " <> show (length other))
  it "finds no pipelines in a file that does not typecheck" $ do
    a <- analyseFixture libDir "TypeMismatch.hs" >>= succeeded
    analysisFlows a `shouldBe` []
