-- |
-- Module      : HInsight.SpliceGhcSpec
-- Description : Tests of the splice pass that run a real GHC.
--
-- They use the library directory in @HINSIGHT_GHC_LIBDIR@ when it is set, and
-- otherwise ask @ghc-9.10.3@. If neither works they are reported as pending.
module HInsight.SpliceGhcSpec (spec) where

import Control.Exception (finally)
import Data.Text (Text)
import HInsight
import HInsight.Environment (mkComponentName, mkEnvironment, mkGhcFlag, mkProjectRoot, pinnedGhcVersion)
import HInsight.Ghc.Session (ghcDeep, ghcDeepIn)
import HInsight.Source (renderPosition, spanStart)
import HInsight.Support (ok)
import HInsight.Toolchain (findGhcLibDir)
import System.Directory (canonicalizePath, createDirectoryIfMissing, getTemporaryDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.Process (getCurrentPid)
import Test.Hspec

splicesOfPath :: FilePath -> FilePath -> IO Splices
splicesOfPath libDir path = do
  dir <- ok (mkLibDir libDir)
  file <- ok (mkSourceFile path)
  splicesFor (ghcDeep dir) file >>= either (fail . show) pure

-- | Where, what kind and what was produced; the source is left out because it
-- holds names with GHC's uniques.
outline :: Splice -> (Text, SpliceKind, [Text])
outline s = (renderPosition (spanStart (spliceSpan s)), spliceKind s, spliceResult s)

spec :: Spec
spec = describe "ghcDeep splices" $ do
  libDir <- runIO findGhcLibDir
  maybe
    (it "reads real splices" (pendingWith "ghc-9.10.3 was not found; install it or set HINSIGHT_GHC_LIBDIR"))
    realSpec
    libDir

realSpec :: FilePath -> Spec
realSpec libDir = do
  it "finds the eleven splices of the spike's Use.hs, by position, with what each produced" $ do
    ss <- splicesOfPath libDir "test/fixtures/splices/Use.hs"
    splicesUnread ss `shouldBe` 0
    splicesBlocked ss `shouldBe` 0
    map outline (splicesFound ss)
      `shouldBe` [ ("8:6", ExpressionSplice, ["7"]),
                   ("11:6", ExpressionSplice, ["(1 + 2)"]),
                   ("14:7", ExpressionSplice, ["1"]),
                   ("16:2", DeclarationSplice, ["k1 :: Integer", "k1 = 10"]),
                   ("18:1", DeclarationSplice, ["k2 :: Integer", "k2 = 20"]),
                   ("20:2", DeclarationSplice, ["k3 :: Integer", "k3 = 30"]),
                   ("23:13", ExpressionSplice, ["(3 + 3)"]),
                   ("25:7", TypeSplice, ["(Int, Bool)"]),
                   ("29:4", PatternSplice, ["Just _"]),
                   ("33:10", ExpressionSplice, ["\"hello\""]),
                   ("36:18", ExpressionSplice, ["9"])
                 ]
  it "notes the splices nested in a splice, as GHC printed them" $ do
    ss <- splicesOfPath libDir "test/fixtures/splices/Use.hs"
    let nested = [(renderPosition (spanStart (spliceSpan s)), length (splicePending s)) | s <- splicesFound ss, not (null (splicePending s))]
    nested `shouldBe` [("11:6", 1), ("20:2", 1)]
  it "still reports the splice that ran before a type error" $ do
    ss <- splicesOfPath libDir "test/fixtures/splices/Bad.hs"
    map outline (splicesFound ss) `shouldBe` [("7:8", ExpressionSplice, ["\"x\""])]
    splicesBlocked ss `shouldBe` 0
  it "gives no splices for a file without any" $ do
    ss <- splicesOfPath libDir "test/fixtures/Ok.hs"
    ss `shouldBe` emptySplices
  it "says GHC was blocked, and why, for a file that does not parse" $ do
    -- Written at run time: a file that does not parse cannot sit among the
    -- sources the linters read.
    tmp <- getTemporaryDirectory
    pid <- getCurrentPid
    let folder = tmp </> ("hinsight-splice-parse-error-" <> show pid)
        path = folder </> "ParseError.hs"
    createDirectoryIfMissing True folder
    writeFile path "module ParseError where\n\nbroken :: Int\nbroken = (1 +\n"
    ss <- splicesOfPath libDir path `finally` removeDirectoryRecursive folder
    splicesFound ss `shouldBe` []
    splicesBlocked ss `shouldSatisfy` (>= 1)
  it "gives the same splices inside a project environment" $ do
    root <- canonicalizePath "test/fixtures/splices" >>= ok . mkProjectRoot
    component <- ok (mkComponentName "fixture")
    flag <- ok (mkGhcFlag "-i.")
    env <- ok (mkEnvironment component root pinnedGhcVersion [flag])
    dir <- ok (mkLibDir libDir)
    file <- ok (mkSourceFile "test/fixtures/splices/Use.hs")
    inEnv <- splicesFor (ghcDeepIn dir env) file >>= either (fail . show) pure
    plain <- splicesOfPath libDir "test/fixtures/splices/Use.hs"
    map outline (splicesFound inEnv) `shouldBe` map outline (splicesFound plain)
