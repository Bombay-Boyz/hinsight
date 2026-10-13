module HInsight.Tadka.CliSpec (spec) where

import HInsight.Tadka.Cli
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

spec :: Spec
spec = describe "parseCommand" $ do
  it "draws graphically by default" $
    parseCommand ["a.hs"] `shouldBe` Run Graphical "a.hs"
  it "takes a format after --format" $
    parseCommand ["--format", "json", "a.hs"] `shouldBe` Run Json "a.hs"
  it "takes a format written with an equals sign" $
    parseCommand ["a.hs", "--format=narratable"] `shouldBe` Run Narratable "a.hs"
  it "names the formats it knows when given another" $
    parseCommand ["--format", "svg", "a.hs"]
      `shouldBe` BadUsage "unknown format svg; use graphical, narratable or json"
  it "needs a value after --format" $
    parseCommand ["a.hs", "--format"] `shouldBe` BadUsage "--format needs a value"
  it "refuses a format given twice" $
    parseCommand ["--format=json", "--format", "json", "a.hs"] `shouldBe` BadUsage "--format was given twice"
  it "asks for a file when there is none" $
    parseCommand ["--format", "json"] `shouldBe` BadUsage "no file was given"
  it "refuses a second file" $
    parseCommand ["a.hs", "b.hs"] `shouldBe` BadUsage "expected exactly one file but got more"
  it "rejects an unknown option and names it" $
    parseCommand ["--verbose", "a.hs"] `shouldBe` BadUsage "unknown option --verbose"
  it "lets help win over everything else" $
    parseCommand ["--format", "svg", "--help"] `shouldBe` ShowHelp
  prop "accepts every format name with any file that does not start with a dash" $
    forAll (elements [(Graphical, "graphical"), (Narratable, "narratable"), (Json, "json")]) $ \(format, name) ->
      forAll (listOf1 (elements ['a' .. 'z'])) $ \file ->
        parseCommand ["--format=" <> name, file] === Run format file
