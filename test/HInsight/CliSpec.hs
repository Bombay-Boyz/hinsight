module HInsight.CliSpec (spec) where

import Data.Text qualified as T
import HInsight.Cli
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

spec :: Spec
spec = describe "parseCommand" $ do
  it "asks for a file when there are no arguments" $
    parseCommand [] `shouldBe` BadUsage "no file was given"
  it "takes a single file" $
    parseCommand ["Main.hs"] `shouldBe` Analyse "Main.hs"
  it "passes an empty path through, so the library can name the problem" $
    parseCommand [""] `shouldBe` Analyse ""
  it "recognises both help flags" $ do
    parseCommand ["-h"] `shouldBe` ShowHelp
    parseCommand ["--help"] `shouldBe` ShowHelp
  it "lets help win over other arguments" $
    parseCommand ["a.hs", "--help", "b.hs"] `shouldBe` ShowHelp
  it "rejects an unknown option and names it" $
    parseCommand ["--verbose"] `shouldBe` BadUsage "unknown option --verbose"
  it "rejects two files and gives the count" $
    parseCommand ["a.hs", "b.hs"] `shouldBe` BadUsage "expected exactly one file but got 2"
  prop "never accepts more than one file" $ \a b rest ->
    notHelp (a : b : rest) ==> case parseCommand (a : b : rest) of
      BadUsage _ -> True
      _ -> False
  prop "accepts any single argument that does not start with a dash" $ \(NonEmpty s) ->
    take 1 s /= "-" ==> parseCommand [s] === Analyse s
  describe "usageText" $
    it "names the program and the pinned compiler" $ do
      usageText `shouldSatisfy` T.isInfixOf "hinsight-demo"
      usageText `shouldSatisfy` T.isInfixOf "ghc-9.10.3"
  where
    notHelp :: [String] -> Bool
    notHelp = not . any (`elem` ["-h", "--help"])
