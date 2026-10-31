-- |
-- Module      : HInsight.CoreGhcSpec
-- Description : Tests of the Core pass that run a real GHC.
--
-- They use the library directory in @HINSIGHT_GHC_LIBDIR@ when it is set, and
-- otherwise ask @ghc-9.10.3@. If neither works they are reported as pending.
module HInsight.CoreGhcSpec (spec) where

import Data.List (isPrefixOf, sort)
import Data.Text (Text)
import HInsight
import HInsight.Environment (mkComponentName, mkEnvironment, mkGhcFlag, mkProjectRoot, pinnedGhcVersion)
import HInsight.Ghc.Session (ghcDeep, ghcDeepIn)
import HInsight.Support (ok)
import HInsight.Toolchain (findGhcLibDir)
import System.Directory (canonicalizePath, doesDirectoryExist, getTemporaryDirectory, listDirectory)
import System.FilePath ((</>))
import Test.Hspec

coreOfPath :: FilePath -> Optimisation -> FilePath -> IO CoreDump
coreOfPath libDir optimisation = coreWith libDir (coreOnly optimisation)

coreWith :: FilePath -> CoreRequest -> FilePath -> IO CoreDump
coreWith libDir request path = do
  dir <- ok (mkLibDir libDir)
  file <- ok (mkSourceFile path)
  coreOf (ghcDeep dir) request file >>= either (fail . show) pure

stgSections :: CoreDump -> [CoreSection]
stgSections = filter ((== FinalStg) . sectionKind) . coreSections

names :: CoreDump -> [Text]
names = map sectionName . coreSections

-- | Every file under a folder, by relative path.
filesUnder :: FilePath -> IO [FilePath]
filesUnder root = go ""
  where
    go :: FilePath -> IO [FilePath]
    go relative = do
      entries <- listDirectory (root </> relative)
      concat <$> traverse (visit relative) entries
    visit :: FilePath -> FilePath -> IO [FilePath]
    visit relative entry = do
      let here = relative </> entry
      isDirectory <- doesDirectoryExist (root </> here)
      if isDirectory then go here else pure [here]

spec :: Spec
spec = describe "ghcDeep Core" $ do
  libDir <- runIO findGhcLibDir
  maybe
    (it "reads real Core" (pendingWith "ghc-9.10.3 was not found; install it or set HINSIGHT_GHC_LIBDIR"))
    realSpec
    libDir

realSpec :: FilePath -> Spec
realSpec libDir = do
  it "optimised: gives the Core, the specialisation and the rules that fired, none unread" $ do
    d <- coreOfPath libDir Optimised "test/fixtures/core/Spec.hs"
    names d `shouldSatisfy` elem "Tidy Core"
    coreRules d `shouldSatisfy` elem (RuleFiring "mymap/mymap" "Spec")
    coreUnread d `shouldBe` 0
    coreBlocked d `shouldBe` 0
  it "unoptimised: gives the Core and no rule firing" $ do
    d <- coreOfPath libDir Unoptimised "test/fixtures/core/Spec.hs"
    names d `shouldSatisfy` elem "Tidy Core"
    coreRules d `shouldBe` []
    coreRequest d `shouldBe` coreOnly Unoptimised
  it "coreModeLeavesNoFiles: the project folder, the working folder and the temporary folder are as they were" $ do
    temporary <- getTemporaryDirectory
    let mine = filter ("hinsight-core" `isPrefixOf`)
        snapshot = (,,) <$> filesUnder "test/fixtures/core" <*> listDirectory "." <*> (mine <$> listDirectory temporary)
    untouched <- snapshot
    _ <- coreOfPath libDir Optimised "test/fixtures/core/Spec.hs"
    _ <- coreOfPath libDir Unoptimised "test/fixtures/core/UseDep.hs"
    _ <- coreWith libDir (CoreRequest Optimised CoreAndStg) "test/fixtures/core/UseDep.hs"
    left <- snapshot
    (\(a, b, c) -> (sort a, sort b, sort c)) left `shouldBe` (\(a, b, c) -> (sort a, sort b, sort c)) untouched
  it "makes the Core of a file that imports a module of its project" $ do
    d <- coreOfPath libDir Optimised "test/fixtures/core/UseDep.hs"
    names d `shouldSatisfy` elem "Tidy Core"
    coreBlocked d `shouldBe` 0
  it "gives no STG unless asked" $ do
    d <- coreOfPath libDir Optimised "test/fixtures/core/Spec.hs"
    stgSections d `shouldBe` []
  it "gives the STG when asked, beside the Core" $ do
    d <- coreWith libDir (CoreRequest Optimised CoreAndStg) "test/fixtures/core/Spec.hs"
    map sectionKind (coreSections d) `shouldSatisfy` (\ks -> TidyCore `elem` ks && FinalStg `elem` ks)
    coreUnread d `shouldBe` 0
    coreBlocked d `shouldBe` 0
  it "gives the STG of the file alone, not of its project imports" $ do
    d <- coreWith libDir (CoreRequest Optimised CoreAndStg) "test/fixtures/core/UseDep.hs"
    length (stgSections d) `shouldBe` 1
  it "gives no STG for a file that does not typecheck, and says GHC was blocked" $ do
    d <- coreWith libDir (CoreRequest Optimised CoreAndStg) "test/fixtures/TypeMismatch.hs"
    coreSections d `shouldBe` []
    coreBlocked d `shouldSatisfy` (>= 1)
  it "makes no Core for a file that does not typecheck, and says GHC was blocked" $ do
    d <- coreOfPath libDir Optimised "test/fixtures/TypeMismatch.hs"
    coreSections d `shouldBe` []
    coreBlocked d `shouldSatisfy` (>= 1)
  it "gives the same rules inside a project environment" $ do
    root <- canonicalizePath "test/fixtures/core" >>= ok . mkProjectRoot
    component <- ok (mkComponentName "fixture")
    flag <- ok (mkGhcFlag "-i.")
    env <- ok (mkEnvironment component root pinnedGhcVersion [flag])
    dir <- ok (mkLibDir libDir)
    file <- ok (mkSourceFile "test/fixtures/core/Spec.hs")
    inEnv <- coreOf (ghcDeepIn dir env) (coreOnly Optimised) file >>= either (fail . show) pure
    plain <- coreOfPath libDir Optimised "test/fixtures/core/Spec.hs"
    sort (coreRules inEnv) `shouldBe` sort (coreRules plain)
    names inEnv `shouldBe` names plain
