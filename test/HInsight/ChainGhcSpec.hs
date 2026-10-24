-- |
-- Module      : HInsight.ChainGhcSpec
-- Description : Tests of the trace pass that run a real GHC.
--
-- They use the library directory in @HINSIGHT_GHC_LIBDIR@ when it is set, and
-- otherwise ask @ghc-9.10.3@. If neither works they are reported as pending.
module HInsight.ChainGhcSpec (spec) where

import Control.Exception (finally)
import Data.Maybe (listToMaybe)
import Data.Text qualified as T
import HInsight
import HInsight.Environment (mkComponentName, mkEnvironment, mkGhcFlag, mkProjectRoot, pinnedGhcVersion)
import HInsight.Ghc.Session (ghcDeep, ghcDeepIn, ghcInsight)
import HInsight.Support (ok)
import HInsight.Toolchain (findGhcLibDir)
import System.Directory (canonicalizePath, createDirectoryIfMissing, getTemporaryDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.Process (getCurrentPid)
import Test.Hspec

chainsOfFixture :: FilePath -> ChainView -> FilePath -> IO Chains
chainsOfFixture libDir view name = chainsOfPath libDir view ("test/fixtures/" <> name)

chainsOfPath :: FilePath -> ChainView -> FilePath -> IO Chains
chainsOfPath libDir view path = do
  dir <- ok (mkLibDir libDir)
  file <- ok (mkSourceFile path)
  chainsFor (ghcDeep dir) view file >>= either (fail . show) pure

mismatchCount :: FilePath -> FilePath -> IO Int
mismatchCount libDir name = do
  dir <- ok (mkLibDir libDir)
  file <- ok (mkSourceFile ("test/fixtures/" <> name))
  result <- analyseFile (ghcInsight dir) file
  analysis <- either (fail . show) pure result
  pure (length (analysisMismatches analysis))

spec :: Spec
spec = describe "ghcDeep" $ do
  libDir <- runIO findGhcLibDir
  maybe
    (it "follows real errors" (pendingWith "ghc-9.10.3 was not found; install it or set HINSIGHT_GHC_LIBDIR"))
    realSpec
    libDir

realSpec :: FilePath -> Spec
realSpec libDir = do
  it "gives one chain for the mismatch of the Pair program, ending at GHC's deferral" $ do
    cs <- chainsOfFixture libDir FollowingVariables "ChainPair.hs"
    chainsMissing cs `shouldBe` 0
    case chainsFound cs of
      [c] -> do
        fmap chainStepSummary (listToMaybe (reverse (chainSteps c)))
          `shouldSatisfy` maybe False (T.isPrefixOf "GHC deferred")
        length (chainSteps c) `shouldSatisfy` (> 1)
      other -> expectationFailure ("expected one chain, got " <> show (length other))
  it "shows at least as many steps for the whole binding as for the variables" $ do
    following <- chainsOfFixture libDir FollowingVariables "ChainPair.hs"
    whole <- chainsOfFixture libDir WholeBinding "ChainPair.hs"
    let count = sum . map (length . chainSteps) . chainsFound
    count whole `shouldSatisfy` (>= count following)
  it "accounts for every error of a rigid-variable mismatch, with a chain or without" $ do
    cs <- chainsOfFixture libDir FollowingVariables "RigidVariable.hs"
    errors <- mismatchCount libDir "RigidVariable.hs"
    length (chainsFound cs) + chainsMissing cs `shouldBe` errors
    mapM_ (\c -> chainSteps c `shouldSatisfy` (not . null)) (chainsFound cs)
    mapM_ (\c -> fmap chainStepSummary (listToMaybe (reverse (chainSteps c))) `shouldSatisfy` maybe False (T.isPrefixOf "GHC deferred")) (chainsFound cs)
  it "gives no chains and counts the error for a file that does not parse" $ do
    -- Written at run time: a file that does not parse cannot sit among the
    -- sources the linters read.
    tmp <- getTemporaryDirectory
    pid <- getCurrentPid
    let folder = tmp </> ("hinsight-parse-error-" <> show pid)
        path = folder </> "ParseError.hs"
    createDirectoryIfMissing True folder
    writeFile path "module ParseError where\n\nbroken :: Int\nbroken = (1 +\n"
    cs <- chainsOfPath libDir FollowingVariables path `finally` removeDirectoryRecursive folder
    chainsFound cs `shouldBe` []
    chainsMissing cs `shouldSatisfy` (>= 1)
  it "gives the same chain inside a project environment" $ do
    root <- canonicalizePath "test/fixtures" >>= ok . mkProjectRoot
    component <- ok (mkComponentName "fixture")
    flag <- ok (mkGhcFlag "-i.")
    env <- ok (mkEnvironment component root pinnedGhcVersion [flag])
    dir <- ok (mkLibDir libDir)
    file <- ok (mkSourceFile "test/fixtures/ChainPair.hs")
    inEnv <- chainsFor (ghcDeepIn dir env) FollowingVariables file >>= either (fail . show) pure
    plain <- chainsOfFixture libDir FollowingVariables "ChainPair.hs"
    -- The variable names carry GHC's uniques and the file name is spelled
    -- differently, so only what is stable is compared: where each step is, how
    -- many there are, and how many records were left out.
    let shape = map (\c -> (map chainStepSpan (chainSteps c), chainOmitted c)) . chainsFound
    shape inEnv `shouldBe` shape plain
    chainsMissing inEnv `shouldBe` chainsMissing plain
  it "gives no chains for a file without errors" $ do
    cs <- chainsOfFixture libDir FollowingVariables "Ok.hs"
    cs `shouldBe` emptyChains
