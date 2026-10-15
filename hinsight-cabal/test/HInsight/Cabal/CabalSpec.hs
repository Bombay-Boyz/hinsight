-- |
-- Module      : HInsight.Cabal.CabalSpec
-- Description : Tests that run a real cabal and a real GHC.
--
-- Each project is written into a temporary folder, so no fixture can be found
-- by accident from the repository's own @cabal.project@. The tests are
-- reported as pending when @cabal@ or GHC 9.10.3 is missing, as the other
-- real-GHC tests are. They need no network: the projects depend on @base@
-- only.
module HInsight.Cabal.CabalSpec (spec) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM, unless, void)
import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as T
import HInsight
import HInsight.Cabal (cabalEnvironment)
import HInsight.Environment (Environment, environmentComponent, environmentFlags, environmentRoot, projectRootPath, unComponentName, unGhcFlag)
import HInsight.Ghc.Session (ghcInsightIn)
import HInsight.Toolchain (findGhcLibDir)
import HInsight.TypeText (unTypeText)
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesDirectoryExist, findExecutable, listDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

-- | A project with one library, whose module has an error that needs
-- @LambdaCase@ to be explained.
withProject :: FilePath -> [Text] -> (FilePath -> IO a) -> IO a
withProject folder extensions act =
  withSystemTempDirectory "hinsight-cabal-test" $ \root -> do
    let dir = root </> folder
    createDirectoryIfMissing True (dir </> "src")
    writeFile (dir </> "cabal.project") "packages: .\nwith-compiler: ghc-9.10.3\n"
    writeFile (dir </> "one.cabal") (cabalFile extensions)
    writeFile (dir </> "src" </> "Lib.hs") libSource
    act dir

cabalFile :: [Text] -> String
cabalFile extensions =
  unlines
    [ "cabal-version: 3.0",
      "name: one",
      "version: 0.1.0.0",
      "build-type: Simple",
      "library",
      "  hs-source-dirs: src",
      "  exposed-modules: Lib",
      "  build-depends: base",
      "  default-language: GHC2021",
      "  default-extensions: " <> T.unpack (T.intercalate ", " extensions)
    ]

libSource :: String
libSource =
  unlines
    [ "module Lib (pick) where",
      "",
      "pick :: Maybe Int -> String",
      "pick = \\case",
      "  Just n -> n",
      "  Nothing -> \"\""
    ]

captured :: FilePath -> IO Environment
captured file = cabalEnvironment file >>= either (fail . T.unpack . renderError) pure

flagsOf :: Environment -> [Text]
flagsOf = map unGhcFlag . environmentFlags

-- | The names of everything under a folder, in order.
listing :: FilePath -> IO [FilePath]
listing dir = do
  entries <- sort <$> listDirectory dir
  fmap concat . forM entries $ \entry -> do
    isDir <- doesDirectoryExist (dir </> entry)
    if isDir then (entry :) . map (entry </>) <$> listing (dir </> entry) else pure [entry]

spec :: Spec
spec = describe "cabalEnvironment" $ do
  cabal <- runIO (findExecutable "cabal")
  libDir <- runIO findGhcLibDir
  case (cabal, libDir) of
    (Just _, Just dir) -> cabalSpec dir
    _ -> it "captures a component" (pendingWith "cabal or ghc-9.10.3 was not found; install them or set HINSIGHT_GHC_LIBDIR")

cabalSpec :: FilePath -> Spec
cabalSpec libDir = do
  it "captures the root, the default extensions and the name of the component" $
    withProject "one" ["LambdaCase"] $ \dir -> do
      env <- captured (dir </> "src" </> "Lib.hs")
      expected <- canonicalizePath dir
      projectRootPath (environmentRoot env) `shouldBe` expected
      flagsOf env `shouldContain` ["-XLambdaCase"]
      T.unpack (unComponentName (environmentComponent env)) `shouldContain` "one"
  it "analyses a file with the flags cabal gave, end to end" $
    withProject "one" ["LambdaCase"] $ \dir -> do
      env <- captured (dir </> "src" </> "Lib.hs")
      mkLib <- either (fail . show) pure (mkLibDir libDir)
      file <- either (fail . show) pure (mkSourceFile (dir </> "src" </> "Lib.hs"))
      result <- analyseFile (ghcInsightIn mkLib env) file
      case result of
        Left err -> expectationFailure (T.unpack (renderError err))
        Right analysis -> case analysisMismatches analysis of
          [m] -> unTypeText (explanationActual m) `shouldBe` "Int"
          other -> expectationFailure ("expected exactly one mismatch, found " <> show (length other))
  it "reports a file with no cabal project above it" $
    withSystemTempDirectory "hinsight-lone" $ \root -> do
      let file = root </> "Lone.hs"
      writeFile file "module Lone () where\n"
      result <- cabalEnvironment file
      void result `shouldBe` Left (EnvironmentFailure (NoCabalProject file))
  it "editedCabalFileIsSeen: the next capture reflects a changed default extension" $
    withProject "one" ["LambdaCase"] $ \dir -> do
      let file = dir </> "src" </> "Lib.hs"
      atFirst <- captured file
      writeFile (dir </> "one.cabal") (cabalFile ["LambdaCase", "OverloadedStrings"])
      later <- captured file
      flagsOf atFirst `shouldNotContain` ["-XOverloadedStrings"]
      flagsOf later `shouldContain` ["-XOverloadedStrings"]
  it "captureLeavesProjectBuildAlone: the project folder is the same after a capture" $
    withProject "one" ["LambdaCase"] $ \dir -> do
      atFirst <- listing dir
      _ <- captured (dir </> "src" </> "Lib.hs")
      later <- listing dir
      later `shouldBe` atFirst
  it "spaceInPathAnalyses: captures a project in a folder whose name has a space" $
    withProject "my project" ["LambdaCase"] $ \dir -> do
      env <- captured (dir </> "src" </> "Lib.hs")
      flagsOf env `shouldContain` ["-XLambdaCase"]
  it "twoAnalysesAtOnce: two simultaneous captures of one project both succeed" $
    withProject "one" ["LambdaCase"] $ \dir -> do
      let file = dir </> "src" </> "Lib.hs"
      done <- newEmptyMVar
      mapM_ (\_ -> forkIO (cabalEnvironment file >>= putMVar done)) [1 :: Int, 2]
      first <- takeMVar done
      second <- takeMVar done
      unless (isRight' first && isRight' second) $
        expectationFailure ("a capture failed: " <> show (either renderError (const "ok") first, either renderError (const "ok") second))
  where
    isRight' :: Either a b -> Bool
    isRight' = either (const False) (const True)
