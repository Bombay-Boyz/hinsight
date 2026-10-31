module HInsight.CoreSpec (spec) where

import Data.Text (Text)
import Data.Text qualified as T
import HInsight.Core
import HInsight.Trace (parseRecord)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

-- | The head of the Tidy Core dump GHC 9.10.3 logged for the spike's @Spec.hs@.
tidyCore :: Text
tidyCore =
  T.intercalate
    "\n"
    [ "==================== Tidy Core ====================",
      "Result size of Tidy Core",
      "  = {terms: 238, types: 138, coercions: 10, joins: 2/2}",
      "",
      "-- RHS size: {terms: 3, types: 3, coercions: 2, joins: 0/0}",
      "area [InlPrag=INLINE] :: forall a. Shape a => a -> Double",
      ""
    ]

stgSection :: Text
stgSection =
  T.intercalate
    "\n"
    [ "==================== Final STG: ====================",
      "area = \\r [d] d",
      ""
    ]

-- | Rule firings GHC 9.10.3 logged for the same file, in the order logged.
firings :: [Text]
firings =
  [ "Rule fired: Class op * (BUILTIN)",
    "Rule fired: Class op + (BUILTIN)",
    "Rule fired: mymap/mymap (Spec)",
    "Rule fired: fold/build (GHC.Internal.Base)",
    "Rule fired: Int# -> Integer -> Double# (GHC.Internal.Float)",
    "Rule fired: fold/build (GHC.Internal.Base)"
  ]

rule :: Text -> Text -> RuleFiring
rule = RuleFiring

spec :: Spec
spec = describe "Core" $ do
  describe "sections, as GHC 9.10.3 printed them" $ do
    it "reads the name of the header and keeps the body as printed" $
      case readCore tidyCore of
        ReadSection s -> do
          sectionName s `shouldBe` "Tidy Core"
          T.lines (sectionBody s)
            `shouldBe` [ "Result size of Tidy Core",
                         "  = {terms: 238, types: 138, coercions: 10, joins: 2/2}",
                         "",
                         "-- RHS size: {terms: 3, types: 3, coercions: 2, joins: 0/0}",
                         "area [InlPrag=INLINE] :: forall a. Shape a => a -> Double"
                       ]
        other -> expectationFailure (show other)
    it "keeps a colon that GHC puts in a name" $
      fmap sectionName (asSection (readCore "==================== Final STG: ====================\nx = y")) `shouldBe` Just "Final STG:"
    it "reads a header with a blank line before it" $
      fmap sectionName (asSection (readCore "\n==================== Specialise ====================\nx")) `shouldBe` Just "Specialise"
    it "counts a header with no name instead of dropping it" $ do
      readCore "==== ====\nx" `shouldSatisfy` isUnread
      readCore "========\nx" `shouldSatisfy` isUnread
  describe "rule firings" $ do
    it "reads the name and the defining module" $
      readCore (at 2) `shouldBe` ReadRules [rule "mymap/mymap" "Spec"]
    it "reads a built-in rule, and a name with spaces and symbols" $ do
      readCore (at 0) `shouldBe` ReadRules [rule "Class op *" "BUILTIN"]
      readCore (at 4) `shouldBe` ReadRules [rule "Int# -> Integer -> Double#" "GHC.Internal.Float"]
    it "takes the last parenthesised word for the module" $
      readCore "Rule fired: f (g) x (M)" `shouldBe` ReadRules [rule "f (g) x" "M"]
    it "reads several lines of one message" $
      readCore (T.unlines (take 2 firings)) `shouldBe` ReadRules [rule "Class op *" "BUILTIN", rule "Class op +" "BUILTIN"]
    it "counts a line that is not a rule firing instead of dropping it" $ do
      readCore "Rule fired: broken" `shouldSatisfy` isUnread
      readCore "Rule fired: a (b c)" `shouldSatisfy` isUnread
      readCore "Rule fired: (M)" `shouldSatisfy` isUnread
  describe "messages that are not Core" $ do
    it "are not taken for it" $ do
      readCore "[1 of 1] Compiling Spec             ( src/Spec.hs, nothing )" `shouldBe` NotCore
      readCore "Use.hs:8:6-24: Splicing expression litE (integerL 7) ======> 7" `shouldBe` NotCore
      readCore "" `shouldBe` NotCore
    it "are left out by the filter the capture uses" $ do
      isCoreMessage (parseRecord tidyCore) `shouldBe` True
      isCoreMessage (parseRecord (at 0)) `shouldBe` True
      isCoreMessage (parseRecord "[1 of 1] Compiling Spec") `shouldBe` False
  describe "buildCore" $ do
    it "keeps sections and rules in the order GHC logged them" $ do
      let d = buildCore (coreOnly Optimised) (take 3 firings <> [tidyCore] <> drop 3 firings)
      map sectionName (coreSections d) `shouldBe` ["Tidy Core"]
      map ruleName (coreRules d) `shouldBe` ["Class op *", "Class op +", "mymap/mymap", "fold/build", "Int# -> Integer -> Double#", "fold/build"]
      coreUnread d `shouldBe` 0
      coreBlocked d `shouldBe` 0
    it "counts what it could not read" $
      coreUnread (buildCore (coreOnly Optimised) ["Rule fired: broken", "==== ====", tidyCore]) `shouldBe` 2
    it "gives nothing for no messages, and says which mode was asked" $
      buildCore (coreOnly Unoptimised) [] `shouldBe` emptyCore (coreOnly Unoptimised)
    prop "is total, and every message is a section, rules, unread or not Core" $
      forAll (listOf (elements (tidyCore : firings <> ["", "x", "====", "Rule fired: ", "Rule fired: x (y)\nz"]))) $ \ms ->
        let d = buildCore (coreOnly Optimised) ms
         in length (coreSections d) + coreUnread d <= length ms
  describe "sectionKind" $
    it "names the kinds by the headers GHC printed, and keeps the rest" $ do
      map (sectionKind . (`CoreSection` "")) ["Tidy Core", "Final STG:", "Specialise", "Specialise, after x", "Desugar"]
        `shouldBe` [TidyCore, FinalStg, Specialisation, Specialisation, OtherSection]
  describe "STG" $ do
    let asked = CoreRequest Optimised CoreAndStg
    it "reads the STG section when it is logged" $
      map sectionKind (coreSections (buildCore asked [tidyCore, stgSection])) `shouldBe` [TidyCore, FinalStg]
    it "says so when STG was asked and GHC logged none" $
      renderCore (buildCore asked [tidyCore]) `shouldSatisfy` T.isInfixOf "  no STG: GHC logged none"
    it "says nothing about STG when it was not asked" $
      renderCore (buildCore (coreOnly Optimised) [tidyCore]) `shouldNotSatisfy` T.isInfixOf "STG"
    it "does not say it when the STG is there" $
      renderCore (buildCore asked [tidyCore, stgSection]) `shouldNotSatisfy` T.isInfixOf "no STG"
    it "names STG in the heading" $
      take 1 (T.lines (renderCore (buildCore asked [stgSection]))) `shouldSatisfy` all (T.isInfixOf ", with STG")
  describe "rendering" $ do
    it "summarises the rules by how often each fired, most first" $ do
      let ls = T.lines (renderCore (buildCore (coreOnly Optimised) firings))
      take 4 ls
        `shouldBe` [ "what GHC logged for the Core of this file (optimised): 0 sections, 6 rule firings",
                     "  2 x fold/build (GHC.Internal.Base)",
                     "  1 x Class op * (BUILTIN)",
                     "  1 x Class op + (BUILTIN)"
                   ]
    it "prints each section under its header, as GHC printed it" $ do
      let ls = T.lines (renderCore (buildCore (coreOnly Unoptimised) [tidyCore]))
      take 1 ls `shouldBe` ["what GHC logged for the Core of this file (unoptimised): 1 section, 0 rule firings"]
      ls `shouldSatisfy` elem "==================== Tidy Core ===================="
      ls `shouldSatisfy` elem "  = {terms: 238, types: 138, coercions: 10, joins: 2/2}"
    it "says what stopped GHC and what could not be read" $ do
      renderCore ((emptyCore (coreOnly Optimised)) {coreBlocked = 2}) `shouldSatisfy` T.isInfixOf "2 error(s) stopped GHC before it made any Core"
      renderCore ((emptyCore (coreOnly Optimised)) {coreUnread = 1}) `shouldSatisfy` T.isInfixOf "1 message(s) could not be read"
  where
    at :: Int -> Text
    at n = foldr const "" (drop n firings)
    asSection :: CoreRead -> Maybe CoreSection
    asSection = \case
      ReadSection s -> Just s
      _ -> Nothing
    isUnread :: CoreRead -> Bool
    isUnread = \case
      UnreadCore _ -> True
      _ -> False
