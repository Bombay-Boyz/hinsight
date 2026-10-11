module HInsight.GraphSpec (spec) where

import Data.List (sort)
import Data.Text qualified as T
import HInsight.Error (DomainError (..))
import HInsight.Graph
import HInsight.Support (genProjectGraph, ok)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

name :: T.Text -> IO ModuleName
name = ok . mkModuleName

-- | A graph from module names and the names each imports.
graphOf :: [(T.Text, [T.Text])] -> IO ProjectGraph
graphOf entries = do
  built <- traverse (\(m, is) -> (,) <$> name m <*> traverse name is) entries
  ok (mkProjectGraph built)

spec :: Spec
spec = do
  describe "mkModuleName" $ do
    it "rejects blank text" $
      mkModuleName "  " `shouldBe` Left (BlankText "module name")
    it "drops surrounding whitespace" $
      fmap unModuleName (mkModuleName "  Data.List ") `shouldBe` Right "Data.List"

  describe "mkProjectGraph" $ do
    it "rejects a module listed twice" $ do
      a <- name "A"
      mkProjectGraph [(a, []), (a, [])] `shouldBe` Left (DuplicateModule "A")
    it "rejects an import of a module that is not in the graph" $ do
      a <- name "A"
      b <- name "B"
      mkProjectGraph [(a, [b])] `shouldBe` Left (UnknownImport "A" "B")
    it "sorts and de-duplicates imports" $ do
      g <- graphOf [("A", ["C", "B", "C"]), ("B", []), ("C", [])]
      a <- name "A"
      map unModuleName (importsOf a g) `shouldBe` ["B", "C"]
      graphImportCount g `shouldBe` 2
    it "has no modules in the empty graph" $
      graphModules emptyGraph `shouldBe` []

  describe "dependencyOrder" $ do
    it "puts every module after the modules it imports" $ do
      g <- graphOf [("App", ["Util"]), ("Util", ["Base"]), ("Base", [])]
      map (map unModuleName) (dependencyOrder g) `shouldBe` [["Base"], ["Util"], ["App"]]
    it "groups modules that import each other" $ do
      g <- graphOf [("A", ["B"]), ("B", ["A"]), ("C", ["A"])]
      map (map unModuleName) (dependencyOrder g) `shouldBe` [["A", "B"], ["C"]]
    prop "lists every module exactly once" $
      forAll genProjectGraph $ \g ->
        sort (concat (dependencyOrder g)) === graphModules g

  describe "importCycles" $ do
    it "finds none in a graph without cycles" $ do
      g <- graphOf [("App", ["Util"]), ("Util", [])]
      importCycles g `shouldBe` []
    it "finds a cycle between two modules" $ do
      g <- graphOf [("A", ["B"]), ("B", ["A"])]
      map (map unModuleName) (importCycles g) `shouldBe` [["A", "B"]]
    it "finds a module that imports itself" $ do
      g <- graphOf [("A", ["A"])]
      map (map unModuleName) (importCycles g) `shouldBe` [["A"]]
    prop "only reports groups that really are cycles" $
      forAll genProjectGraph $ \g ->
        all (\group -> length group > 1 || any (\m -> m `elem` importsOf m g) group) (importCycles g)

  describe "renderGraph" $ do
    it "lists modules in dependency order with their imports" $ do
      g <- graphOf [("App", ["Lib.Util"]), ("Lib.Util", ["Lib.Base"]), ("Lib.Base", [])]
      T.lines (renderGraph g)
        `shouldBe` [ "module graph: 3 modules, 2 imports",
                     "  Lib.Base",
                     "  Lib.Util  imports Lib.Base",
                     "  App       imports Lib.Util"
                   ]
    it "names each import cycle" $ do
      g <- graphOf [("A", ["B"]), ("B", ["A"])]
      T.lines (renderGraph g) `shouldContain` ["import cycle: A, B"]
    it "uses the singular for one module" $ do
      g <- graphOf [("A", [])]
      T.lines (renderGraph g) `shouldBe` ["module graph: 1 module, 0 imports", "  A"]
