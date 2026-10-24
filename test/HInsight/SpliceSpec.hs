module HInsight.SpliceSpec (spec) where

import Data.Text (Text)
import Data.Text qualified as T
import HInsight.Source (renderPosition, spanStart)
import HInsight.Splice
import HInsight.Trace (parseRecord)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

-- | A message from its lines, as GHC logs it (no final newline).
msg :: [Text] -> Text
msg = T.intercalate "\n"

-- | What is compared between readings: kind, position, source, nested notes
-- and result. The raw message is left out because it depends on the width.
shape :: Splice -> (SpliceKind, Text, Text, [Text], [Text])
shape s = (spliceKind s, renderPosition (spanStart (spliceSpan s)), spliceSource s, splicePending s, spliceResult s)

note1, note2 :: Text
note1 = "pending(rn) [<spn_a2Qi, litE (integerL 2)>]"
note2 = "pending(rn) [<spn_a2S2, litE (integerL 1)>]"

-- | The eleven splices of the spike's @Use.hs@, by position.
expected :: [(SpliceKind, Text, Text, [Text], [Text])]
expected =
  [ (ExpressionSplice, "8:6", "litE (integerL 7)", [], ["7"]),
    (ExpressionSplice, "11:6", "[| 1 + $(litE (integerL 2)) |]", [note1], ["(1 + 2)"]),
    (ExpressionSplice, "14:7", "typedOne", [], ["1"]),
    (DeclarationSplice, "16:2", "mkConst \"k1\" 10", [], ["k1 :: Integer", "k1 = 10"]),
    (DeclarationSplice, "18:1", "mkConst \"k2\" 20", [], ["k2 :: Integer", "k2 = 20"]),
    ( DeclarationSplice,
      "20:2",
      "do ds_a2S1 <- mkConst \"k3\" 30 e_a2S3 <- [| $(litE (integerL 1)) |] return ds_a2S1",
      [note2],
      ["k3 :: Integer", "k3 = 30"]
    ),
    (ExpressionSplice, "23:13", "twice [| 3 |]", [], ["(3 + 3)"]),
    (TypeSplice, "25:7", "tupleT2", [], ["(Int, Bool)"]),
    (PatternSplice, "29:4", "justP", [], ["Just _"]),
    (ExpressionSplice, "33:10", "Language.Haskell.TH.Quote.quoteExp str \"hello\"", [], ["\"hello\""]),
    (ExpressionSplice, "36:18", "litE (integerL 9)", [], ["9"])
  ]

-- | The messages GHC 9.10.3 logged for @Use.hs@ at the default width, in the
-- order it logged them.
atDefault :: [Text]
atDefault =
  [ "Use.hs:8:6-24: Splicing expression litE (integerL 7) ======> 7",
    msg ["Use.hs:11:6-39: Splicing expression", "    [| 1 + $(litE (integerL 2)) |]", "    pending(rn) [<spn_a2Qi, litE (integerL 2)>]", "  ======>", "    (1 + 2)"],
    msg ["Use.hs:16:2-18: Splicing declarations", "    mkConst \"k1\" 10", "  ======>", "    k1 :: Integer", "    k1 = 10"],
    msg ["Use.hs:18:1-15: Splicing declarations", "    mkConst \"k2\" 20", "  ======>", "    k2 :: Integer", "    k2 = 20"],
    msg
      [ "Use.hs:20:2-77: Splicing declarations",
        "    do ds_a2S1 <- mkConst \"k3\" 30",
        "       e_a2S3 <- [| $(litE (integerL 1)) |]",
        "                 pending(rn) [<spn_a2S2, litE (integerL 1)>]",
        "       return ds_a2S1",
        "  ======>",
        "    k3 :: Integer",
        "    k3 = 30"
      ],
    "Use.hs:25:7-15: Splicing type tupleT2 ======> (Int, Bool)",
    "Use.hs:23:13-27: Splicing expression twice [| 3 |] ======> (3 + 3)",
    "Use.hs:29:4-10: Splicing pattern justP ======> Just _",
    msg ["Use.hs:33:10-16: Splicing expression", "    Language.Haskell.TH.Quote.quoteExp str \"hello\" ======> \"hello\""],
    "Use.hs:36:18-36: Splicing expression litE (integerL 9) ======> 9",
    "Use.hs:14:7-16: Splicing expression typedOne ======> 1"
  ]

-- | The same at width 200: only the quasi-quote moves.
atWide :: [Text]
atWide = map widen atDefault
  where
    widen :: Text -> Text
    widen m
      | "quoteExp" `T.isInfixOf` m = "Use.hs:33:10-16: Splicing expression Language.Haskell.TH.Quote.quoteExp str \"hello\" ======> \"hello\""
      | otherwise = m

-- | The same at width 40: nearly every message wraps.
atNarrow :: [Text]
atNarrow =
  [ msg ["Use.hs:8:6-24: Splicing expression", "    litE (integerL 7) ======> 7"],
    msg
      [ "Use.hs:11:6-39: Splicing expression",
        "    [| 1 + $(litE",
        "               (integerL 2)) |]",
        "    pending(rn) [<spn_a2Qi, litE",
        "                              (integerL",
        "                                 2)>]",
        "  ======>",
        "    (1 + 2)"
      ],
    msg ["Use.hs:16:2-18: Splicing declarations", "    mkConst \"k1\" 10", "  ======>", "    k1 :: Integer", "    k1 = 10"],
    msg ["Use.hs:18:1-15: Splicing declarations", "    mkConst \"k2\" 20", "  ======>", "    k2 :: Integer", "    k2 = 20"],
    msg
      [ "Use.hs:20:2-77: Splicing declarations",
        "    do ds_a2S1 <- mkConst",
        "                    \"k3\" 30",
        "       e_a2S3 <- [| $(litE",
        "                        (integerL 1)) |]",
        "                 pending(rn) [<spn_a2S2, litE",
        "                                           (integerL",
        "                                              1)>]",
        "       return ds_a2S1",
        "  ======>",
        "    k3 :: Integer",
        "    k3 = 30"
      ],
    msg ["Use.hs:25:7-15: Splicing type", "    tupleT2 ======> (Int, Bool)"],
    msg ["Use.hs:23:13-27: Splicing expression", "    twice [| 3 |]", "  ======>", "    (3 + 3)"],
    msg ["Use.hs:29:4-10: Splicing pattern", "    justP ======> Just _"],
    msg ["Use.hs:33:10-16: Splicing expression", "    Language.Haskell.TH.Quote.quoteExp", "      str \"hello\"", "  ======>", "    \"hello\""],
    msg ["Use.hs:36:18-36: Splicing expression", "    litE (integerL 9) ======> 9"],
    msg ["Use.hs:14:7-16: Splicing expression", "    typedOne ======> 1"]
  ]

spec :: Spec
spec = describe "Splice" $ do
  describe "the splices of the spike's Use.hs, from a real GHC 9.10.3" $ do
    it "gives every kind of splice, by position, at the default width" $ do
      let ss = buildSplices atDefault
      map shape (splicesFound ss) `shouldBe` expected
      splicesUnread ss `shouldBe` 0
      splicesBlocked ss `shouldBe` 0
    it "reads the same at width 200" $
      map shape (splicesFound (buildSplices atWide)) `shouldBe` expected
    it "reads the same at width 40, where nearly every message wraps" $
      map shape (splicesFound (buildSplices atNarrow)) `shouldBe` expected
    it "keeps the message as GHC logged it" $
      map spliceRaw (splicesFound (buildSplices atDefault))
        `shouldBe` map (at atDefault) [0, 1, 10, 2, 3, 4, 6, 5, 7, 8, 9]
  describe "readSplice" $ do
    it "keeps a kind it does not know, as GHC printed it" $
      fmap spliceKind (asSplice (readSplice "A.hs:1:1-3: Splicing frobnicator x ======> y"))
        `shouldBe` Just (OtherSplice "frobnicator")
    it "counts a splice message with no arrow instead of dropping it" $ do
      readSplice "A.hs:1:1-3: Splicing expression x" `shouldSatisfy` isUnread
      splicesUnread (buildSplices ["A.hs:1:1-3: Splicing expression x"]) `shouldBe` 1
    it "counts a splice message with no kind" $
      readSplice "A.hs:1:1-3: Splicing " `shouldSatisfy` isUnread
    it "does not take another message for a splice" $ do
      readSplice "A.hs:1:1-3:: u_tys tclvl 0 Int ~nominal Bool" `shouldBe` NotASplice
      readSplice "Splicing expression x ======> y" `shouldBe` NotASplice
      readSplice "" `shouldBe` NotASplice
    it "leaves a note whose bracket is never closed in the source" $
      fmap (\s -> (spliceSource s, splicePending s)) (asSplice (readSplice "A.hs:1:1-3: Splicing expression f pending(rn) [<x ======> y"))
        `shouldBe` Just ("f pending(rn) [<x", [])
    it "reads an empty result as nothing produced" $
      fmap spliceResult (asSplice (readSplice (msg ["A.hs:1:1-3: Splicing declarations", "    f", "  ======>"])))
        `shouldBe` Just []
    it "joins the lines that continue a declaration and splits those that begin one" $
      fmap spliceResult (asSplice (readSplice (msg ["A.hs:1:1-3: Splicing declarations", "    f", "  ======>", "    f :: Int ->", "           Int", "    f = id"])))
        `shouldBe` Just ["f :: Int -> Int", "f = id"]
  describe "splicesIn, the filter the capture hook uses" $ do
    it "keeps a splice message of the file, spelled any way" $ do
      splicesIn "Use.hs" (parseRecord (head' atDefault)) `shouldBe` True
      splicesIn "./Use.hs" (parseRecord (head' atDefault)) `shouldBe` True
    it "drops a splice message of another file" $
      splicesIn "Other.hs" (parseRecord (head' atDefault)) `shouldBe` False
    it "keeps a splice message it could not read, because it is counted" $
      splicesIn "A.hs" (parseRecord "A.hs:1:1-3: Splicing expression x") `shouldBe` True
    it "drops a message that is not about splices" $
      splicesIn "A.hs" (parseRecord "A.hs:1:1-3:: u_tys tclvl 0 Int ~nominal Bool") `shouldBe` False
  describe "buildSplices" $ do
    it "lists splices by position, and keeps the order GHC ran equal ones in" $ do
      let two = ["A.hs:5:1-3: Splicing expression b ======> 1", "A.hs:2:1-3: Splicing expression a ======> 2", "A.hs:5:1-3: Splicing expression c ======> 3"]
      map spliceSource (splicesFound (buildSplices two)) `shouldBe` ["a", "b", "c"]
    prop "is total on any text" $
      forAll (listOf (elements (atDefault <> atNarrow <> ["", "x", "======>", ": Splicing "]))) $ \ms ->
        length (splicesFound (buildSplices ms)) + splicesUnread (buildSplices ms) <= length ms
  describe "rendering" $ do
    it "shows the position, kind, source and what was produced" $
      T.lines (renderSplices (buildSplices (take 1 atDefault)))
        `shouldBe` ["8:6  expression splice", "    source: litE (integerL 7)", "    produced: 7"]
    it "lists the declarations a declaration splice produced" $
      T.lines (renderSplices (buildSplices [at atDefault 2]))
        `shouldBe` ["16:2  declaration splice", "    source: mkConst \"k1\" 10", "    produced:", "      k1 :: Integer", "      k1 = 10"]
    it "shows GHC's note of a nested splice" $
      renderSplices (buildSplices [at atDefault 1]) `shouldSatisfy` T.isInfixOf ("GHC noted nested splices: " <> note1)
    it "says so when GHC ran no splice" $
      renderSplices emptySplices `shouldBe` "no splices: GHC ran none in this file\n"
    it "says what stopped GHC and what could not be read" $ do
      renderSplices (emptySplices {splicesBlocked = 2}) `shouldBe` "2 error(s) stopped GHC before it could run any splice\n"
      renderSplices (emptySplices {splicesUnread = 1}) `shouldBe` "1 splice message(s) could not be read\n"
  where
    asSplice :: SpliceRead -> Maybe Splice
    asSplice = \case
      SpliceRead s -> Just s
      _ -> Nothing
    isUnread :: SpliceRead -> Bool
    isUnread = \case
      SpliceUnread _ _ -> True
      _ -> False
    head' :: [Text] -> Text
    head' = foldr const ""
    at :: [Text] -> Int -> Text
    at ms n = head' (drop n ms)
