-- |
-- Module      : HInsight.GhcSessionSpec
-- Description : Tests that run a real GHC.
--
-- The checks that need a GHC use the library directory in
-- @HINSIGHT_GHC_LIBDIR@ when it is set, and otherwise ask @ghc-9.10.3@, the
-- compiler this project is pinned to. If neither works they are reported as
-- pending rather than silently passing.
module HInsight.GhcSessionSpec (spec) where

import Control.Exception (IOException, finally, try)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import HInsight
import HInsight.Error (SessionError (..))
import HInsight.Ghc.Session (ghcInsight)
import HInsight.Graph (emptyGraph)
import HInsight.Hole (Locality (..), unRefinementLevel)
import HInsight.Source (positionLine, spanStart, unLine)
import HInsight.Support (ok)
import HInsight.TypeText (unTypeText)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removeDirectoryRecursive)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.Process (getCurrentPid, readProcess)
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
    (\dir -> realGhcSpec dir *> projectSpec dir)
    libDir

-- | A context for a binding with this name, declared at this line as this type.
isSignedAs :: Text -> Int -> Text -> Context -> Bool
isSignedAs name line declared = \case
  InSignedBinding n sp t ->
    unIdentifier n == name
      && unLine (positionLine (spanStart sp)) == line
      && unTypeText t == declared
  _ -> False

-- | An analysis with its module graph cleared: a file that imports nothing
-- still has a graph with one module in it.
withoutGraph :: Analysis -> Analysis
withoutGraph a = a {analysisGraph = emptyGraph}

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
    withoutGraph a `shouldBe` emptyAnalysis
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
  it "still shows pipelines in a file that has a type error elsewhere" $ do
    a <- analyseFixture libDir "PipelineMismatch.hs" >>= succeeded
    length (analysisMismatches a) `shouldBe` 1
    case analysisFlows a of
      [composed] -> do
        pipelineKind composed `shouldBe` Composition
        stageTexts composed `shouldBe` ["words", "map length", "sum"]
        drop 2 (stageTypes composed) `shouldBe` ["[Int] -> Int"]
      other -> expectationFailure ("expected one pipeline, found " <> show (length other))

-- | Project loading: a file is analysed together with the modules it imports.
projectSpec :: FilePath -> Spec
projectSpec libDir = describe "a file that imports modules of its own project" $ do
  it "explains a mismatch that depends on an imported module" $ do
    a <- analyseFixture libDir "project/App.hs" >>= succeeded
    case analysisMismatches a of
      [m] -> do
        unTypeText (explanationActual m) `shouldBe` "Int"
        unTypeText (explanationExpected m) `shouldSatisfy` (`elem` ["String", "[Char]"])
      other -> expectationFailure ("expected exactly one mismatch, found " <> show (length other))
  it "reports the module graph with only the modules the file reaches" $ do
    a <- analyseFixture libDir "project/App.hs" >>= succeeded
    let graph = analysisGraph a
    map unModuleName (graphModules graph) `shouldBe` ["App", "Lib.Base", "Lib.Util"]
    map (map unModuleName) (dependencyOrder graph) `shouldBe` [["Lib.Base"], ["Lib.Util"], ["App"]]
    importCycles graph `shouldBe` []
  it "does not load modules that the file does not import" $ do
    -- Unrelated.hs sits next to App.hs and has a type error of its own.
    a <- analyseFixture libDir "project/App.hs" >>= succeeded
    length (analysisMismatches a) `shouldBe` 1
  it "finds the source folder of a file inside a module hierarchy" $ do
    a <- analyseFixture libDir "project/Lib/Util.hs" >>= succeeded
    (analysisMismatches a, analysisHoles a, analysisUnexplained a) `shouldBe` ([], [], 0)
    map unModuleName (graphModules (analysisGraph a)) `shouldBe` ["Lib.Base", "Lib.Util"]
  it "reports a dependency that does not compile instead of analysing against it" $ do
    r <- analyseFixture libDir "broken/Top.hs"
    case r of
      Left (SessionFailure (DependencyFailed file why)) -> do
        file `shouldBe` "test/fixtures/broken/Top.hs"
        why `shouldSatisfy` T.isInfixOf "Bad"
      other -> expectationFailure ("expected DependencyFailed, got " <> show other)
  it "reports something for an import cycle" $ do
    r <- analyseFixture libDir "cycle/A.hs"
    r `shouldSatisfy` reportsSomething
  it "counts an import of a module that does not exist" $ do
    a <- analyseFixture libDir "missing/UsesMissing.hs" >>= succeeded
    analysisUnexplained a `shouldSatisfy` (>= 1)
  it "sees an edit to an imported module on the next analysis" $ do
    base <- getTemporaryDirectory
    pid <- getCurrentPid
    let dir = base </> ("hinsight-edit-" <> show pid)
    createDirectoryIfMissing True dir
    (`finally` removeDirectoryRecursive dir) $ do
      writeFile (dir </> "Dep.hs") "module Dep (value) where\n\nvalue :: Int\nvalue = 1\n"
      writeFile (dir </> "Entry.hs") "module Entry (n) where\n\nimport Dep (value)\n\nn :: Int\nn = value\n"
      file <- ok (mkSourceFile (dir </> "Entry.hs"))
      dirOk <- ok (mkLibDir libDir)
      initial <- analyseFile (ghcInsight dirOk) file >>= succeeded
      withoutGraph initial `shouldBe` emptyAnalysis
      writeFile (dir </> "Dep.hs") "module Dep (value) where\n\nvalue :: String\nvalue = \"one\"\n"
      edited <- analyseFile (ghcInsight dirOk) file >>= succeeded
      length (analysisMismatches edited) `shouldBe` 1
  where
    reportsSomething :: Either InsightError Analysis -> Bool
    reportsSomething = \case
      Left _ -> True
      Right a -> analysisUnexplained a > 0 || not (null (analysisMismatches a))
