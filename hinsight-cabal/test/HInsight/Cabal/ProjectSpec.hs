-- |
-- Module      : HInsight.Cabal.ProjectSpec
-- Description : Tests of finding the cabal project above a file.
module HInsight.Cabal.ProjectSpec (spec) where

import HInsight.Cabal.Project (findProjectDir)
import System.Directory (canonicalizePath, createDirectoryIfMissing, createFileLink)
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

-- | Create an empty file, and the folders above it.
touch :: FilePath -> IO ()
touch path = createDirectoryIfMissing True (takeDirectory path) *> writeFile path ""

-- | The project of a file, with the folder given back as its real path.
projectOf :: FilePath -> IO (Maybe FilePath)
projectOf = findProjectDir

spec :: Spec
spec = describe "findProjectDir" $ do
  it "finds the folder with a cabal.project above the file" $
    withSystemTempDirectory "hinsight-project" $ \root -> do
      touch (root </> "a" </> "cabal.project")
      touch (root </> "a" </> "pkg" </> "src" </> "X.hs")
      expected <- canonicalizePath (root </> "a")
      projectOf (root </> "a" </> "pkg" </> "src" </> "X.hs") `shouldReturn` Just expected
  it "prefers a cabal.project above a .cabal file nearer the file, as cabal does" $
    withSystemTempDirectory "hinsight-project" $ \root -> do
      touch (root </> "a" </> "cabal.project")
      touch (root </> "a" </> "pkg" </> "pkg.cabal")
      touch (root </> "a" </> "pkg" </> "X.hs")
      expected <- canonicalizePath (root </> "a")
      projectOf (root </> "a" </> "pkg" </> "X.hs") `shouldReturn` Just expected
  it "falls back to the nearest .cabal file when there is no cabal.project" $
    withSystemTempDirectory "hinsight-project" $ \root -> do
      touch (root </> "c" </> "c.cabal")
      touch (root </> "c" </> "src" </> "Y.hs")
      expected <- canonicalizePath (root </> "c")
      projectOf (root </> "c" </> "src" </> "Y.hs") `shouldReturn` Just expected
  it "nestedProjectUsesInnerCabalFile: the inner project wins over the outer one" $
    withSystemTempDirectory "hinsight-project" $ \root -> do
      touch (root </> "o" </> "cabal.project")
      touch (root </> "o" </> "in" </> "cabal.project")
      touch (root </> "o" </> "in" </> "src" </> "Z.hs")
      expected <- canonicalizePath (root </> "o" </> "in")
      projectOf (root </> "o" </> "in" </> "src" </> "Z.hs") `shouldReturn` Just expected
  it "spaceInPathAnalyses: works in a folder whose name has a space" $
    withSystemTempDirectory "hinsight-project" $ \root -> do
      touch (root </> "my project" </> "cabal.project")
      touch (root </> "my project" </> "W.hs")
      expected <- canonicalizePath (root </> "my project")
      projectOf (root </> "my project" </> "W.hs") `shouldReturn` Just expected
  it "symlinkedFileAnalyses: a link to a file belongs to the project of the file it points to" $
    withSystemTempDirectory "hinsight-project" $ \root -> do
      touch (root </> "real" </> "cabal.project")
      touch (root </> "real" </> "V.hs")
      touch (root </> "other" </> "cabal.project")
      createFileLink (root </> "real" </> "V.hs") (root </> "other" </> "V.hs")
      expected <- canonicalizePath (root </> "real")
      projectOf (root </> "other" </> "V.hs") `shouldReturn` Just expected
  it "is nothing when no folder above the file is a project" $
    withSystemTempDirectory "hinsight-project" $ \root -> do
      touch (root </> "d" </> "Lone.hs")
      projectOf (root </> "d" </> "Lone.hs") `shouldReturn` Nothing
