-- |
-- Module      : HInsight.Cabal.CliSpec
-- Description : Tests of the demo's argument parsing.
module HInsight.Cabal.CliSpec (spec) where

import Data.Text qualified as T
import HInsight.Cabal.Cli (Command (..), EnvironmentChoice (..), parseCommand, usageText)
import Test.Hspec

spec :: Spec
spec = describe "parseCommand" $ do
  it "asks cabal when no environment is named" $
    parseCommand ["Foo.hs"] `shouldBe` Run CabalEnvironment "Foo.hs"
  it "reads the environment as a separate argument or with an equals sign, before or after the file" $ do
    parseCommand ["--environment", "header", "Foo.hs"] `shouldBe` Run HeaderEnvironment "Foo.hs"
    parseCommand ["Foo.hs", "--environment=cabal"] `shouldBe` Run CabalEnvironment "Foo.hs"
  it "lets help win over everything else" $
    parseCommand ["--environment", "nonsense", "--help"] `shouldBe` ShowHelp
  it "refuses an unknown environment, naming it and the choices" $
    case parseCommand ["--environment=stack", "Foo.hs"] of
      BadUsage why -> do
        why `shouldSatisfy` T.isInfixOf "stack"
        why `shouldSatisfy` T.isInfixOf "cabal or header"
      other -> expectationFailure (show other)
  it "refuses a missing value, a repeat, a second file, an unknown option and no file" $ do
    parseCommand ["--environment"] `shouldSatisfy` isBad
    parseCommand ["--environment=cabal", "--environment=header", "F.hs"] `shouldSatisfy` isBad
    parseCommand ["A.hs", "B.hs"] `shouldSatisfy` isBad
    parseCommand ["--wat", "A.hs"] `shouldSatisfy` isBad
    parseCommand [] `shouldSatisfy` isBad
  it "documents both environments in the help text" $ do
    usageText `shouldSatisfy` T.isInfixOf "cabal"
    usageText `shouldSatisfy` T.isInfixOf "header"
  where
    isBad :: Command -> Bool
    isBad = \case
      BadUsage _ -> True
      _ -> False
