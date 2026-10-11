module HInsight.ProjectSpec (spec) where

import Data.List (intercalate)
import HInsight.Project (sourceRoot)
import System.FilePath (joinPath, (<.>))
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

spec :: Spec
spec = describe "sourceRoot" $ do
  it "strips the module path from a hierarchical module" $
    sourceRoot "proj/src/Lib/Util.hs" "Lib.Util" `shouldBe` "proj/src"
  it "uses the current folder when the file is at the top of the tree" $
    sourceRoot "Lib/Util.hs" "Lib.Util" `shouldBe` "."
  it "keeps an absolute root" $
    sourceRoot "/work/proj/Lib/Util.hs" "Lib.Util" `shouldBe` "/work/proj"
  it "uses the file's folder for a flat module" $
    sourceRoot "test/fixtures/Ok.hs" "Ok" `shouldBe` "test/fixtures"
  it "falls back to the file's folder when the module name does not match the path" $
    sourceRoot "scripts/run.hs" "Tools.Run" `shouldBe` "scripts"
  it "falls back to the file's folder for a Main module with another file name" $
    sourceRoot "app/entry.hs" "Main" `shouldBe` "app"
  prop "recovers the root of a path built from a module name" $
    forAll genLayout $ \(root, parts) ->
      sourceRoot (joinPath (root <> parts) <.> "hs") (intercalate "." parts)
        === (if null root then "." else joinPath root)

genLayout :: Gen ([String], [String])
genLayout = (,) <$> resize 3 (listOf segment) <*> resize 3 (listOf1 segment)
  where
    segment :: Gen String
    segment = (:) <$> elements ['A' .. 'Z'] <*> resize 4 (listOf (elements ['a' .. 'z']))
