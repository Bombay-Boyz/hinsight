module HInsight.CliSpec (spec) where

import Data.Text qualified as T
import HInsight.Chain (ChainView (..))
import HInsight.Cli
import HInsight.Core (Optimisation (..))
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
    notHelp (a : b : rest) && not (any isChainFlag (a : b : rest)) ==> case parseCommand (a : b : rest) of
      BadUsage _ -> True
      _ -> False
  describe "chains" $ do
    it "asks for the chains that follow the variables" $
      parseCommand ["--chains", "A.hs"] `shouldBe` ExplainChains FollowingVariables "A.hs"
    it "asks for the whole binding, with the flag on either side of the file" $
      parseCommand ["A.hs", "--chains=whole"] `shouldBe` ExplainChains WholeBinding "A.hs"
    it "lets the last chain flag win" $
      parseCommand ["--chains=whole", "--chains", "A.hs"] `shouldBe` ExplainChains FollowingVariables "A.hs"
    it "still needs a file" $
      parseCommand ["--chains"] `shouldBe` BadUsage "no file was given"
    it "still counts files" $
      parseCommand ["--chains", "a.hs", "b.hs"] `shouldBe` BadUsage "expected exactly one file but got 2"
    it "asks for the splices" $
      parseCommand ["--splices", "A.hs"] `shouldBe` ExplainSplices "A.hs"
    it "will not combine the modes" $ do
      parseCommand ["--splices", "--chains", "A.hs"] `shouldBe` BadUsage "choose one of --chains, --splices and --core"
      parseCommand ["--core", "--splices", "A.hs"] `shouldBe` BadUsage "choose one of --chains, --splices and --core"
      parseCommand ["--chains", "--core", "A.hs"] `shouldBe` BadUsage "choose one of --chains, --splices and --core"
    it "asks for the Core, optimised or not, and the last core flag wins" $ do
      parseCommand ["--core", "A.hs"] `shouldBe` ExplainCore Optimised "A.hs"
      parseCommand ["A.hs", "--core=unoptimised"] `shouldBe` ExplainCore Unoptimised "A.hs"
      parseCommand ["--core", "--core=unoptimised", "A.hs"] `shouldBe` ExplainCore Unoptimised "A.hs"
    it "still needs a file for the Core" $
      parseCommand ["--core"] `shouldBe` BadUsage "no file was given"
    it "still needs a file for splices" $
      parseCommand ["--splices"] `shouldBe` BadUsage "no file was given"
    it "names an unknown option even beside a chain flag" $
      parseCommand ["--chains", "--verbose", "A.hs"] `shouldBe` BadUsage "unknown option --verbose"
  prop "accepts any single argument that does not start with a dash" $ \(NonEmpty s) ->
    take 1 s /= "-" ==> parseCommand [s] === Analyse s
  describe "usageText" $ do
    it "names the program and the pinned compiler" $ do
      usageText `shouldSatisfy` T.isInfixOf "hinsight-demo"
      usageText `shouldSatisfy` T.isInfixOf "ghc-9.10.3"
    it "describes the splice and Core options" $ do
      usageText `shouldSatisfy` T.isInfixOf "--splices"
      usageText `shouldSatisfy` T.isInfixOf "--core=unoptimised"
    it "describes both chain views" $ do
      usageText `shouldSatisfy` T.isInfixOf "--chains"
      usageText `shouldSatisfy` T.isInfixOf "--chains=whole"
  where
    isChainFlag :: String -> Bool
    isChainFlag = (`elem` ["--chains", "--chains=whole", "--splices", "--core", "--core=unoptimised"])
    notHelp :: [String] -> Bool
    notHelp = not . any (`elem` ["-h", "--help"])
