module HInsight.TraceSpec (spec) where

import Data.Text (Text)
import Data.Text qualified as T
import HInsight.Error (DomainError (..), TraceError (..), renderDomainError, renderTraceError)
import HInsight.Source (renderPosition, sourceFilePath, spanEnd, spanStart)
import HInsight.Trace
import HInsight.Trace.Clean (cleanBlock, cleanType, residue)
import HInsight.TypeText (TypeText, mkTypeText, unTypeText)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

-- | File, start and end of a record's location, as text.
where' :: TraceRecord -> Maybe (FilePath, Text, Text)
where' r = case recordLocation r of
  Nothing -> Nothing
  Just l ->
    Just
      ( sourceFilePath (locatedFile l),
        renderPosition (spanStart (locatedSpan l)),
        renderPosition (spanEnd (locatedSpan l))
      )

-- | The step of a message that starts with a span, as the typechecker prints it.
stepOf :: Text -> Step
stepOf body = recordStep (parseRecord ("Pair.hs:5:5-23:: " <> body))

mk :: Text -> Maybe TypeText
mk = either (const Nothing) Just . mkTypeText

-- | Offer messages to a capture with the given limit.
run :: Int -> (TraceRecord -> Bool) -> [Text] -> Maybe (Either TraceError Trace)
run n wanted ms = case mkRecordLimit n of
  Left _ -> Nothing
  Right l -> Just (finishCapture (foldl' (flip (capture wanted)) (emptyCapture l) ms))

-- Lines GHC 9.10.3 printed under -ddump-tc-trace -dppr-debug -ferror-spans.
uTys, noCoercion, coerced, filledLine, deferLine, defer2Line, unfilledOk, subTypeLine, instFunLine, instRetLine :: Text
uTys = "u_tys tclvl 0 [] (a_ax0{tv} Nothing [tau:0] :: GHC.Types.Type) ~nominal [] GHC.Types.Int"
noCoercion = "u_tys yields no coercion"
coerced = "u_tys yields coercion: {(co_axa{v} Just GHC.Types.Many [lid[CoVarId]] :: GHC.Prim.~# GHC.Types.Type GHC.Types.Type GHC.Internal.Base.String GHC.Types.Int)}"
filledLine = "found filled tyvar (a_ax0{tv} Nothing [tau:0] :: GHC.Types.Type) :-> GHC.Types.Int"
deferLine = "utype_defer nominal GHC.Internal.Base.String GHC.Types.Int In the second argument of ‘Pair.pair’, namely ‘\"x\"’"
defer2Line = "utype_defer2 {(co_axa{v} Just GHC.Types.Many [lid[CoVarId]] :: GHC.Prim.~# GHC.Types.Type GHC.Types.Type GHC.Internal.Base.String GHC.Types.Int)}"
unfilledOk = "uUnfilledVar2 ok (a_aUq{tv} Nothing [tau:0] :: GHC.Types.Type) :: GHC.Types.Type GHC.Types.Char :: GHC.Types.Type True <*>_N"
subTypeLine = "tc_sub_type (general case) ty_actual   = GHC.Internal.Base.String ty_expected = (a_ax0{tv} Nothing [tau:0] :: GHC.Types.Type)"
instFunLine = "tcInstFun tc_fun Pair.pair fun_sigma forall (a Nothing [sk:2] :: GHC.Types.Type). (a Nothing [sk:2] :: GHC.Types.Type) -> [] (a Nothing [sk:2] :: GHC.Types.Type) fun_ctxt VA"
instRetLine = "tcInstFun:ret [] (a_ax0{tv} Nothing [tau:0] :: GHC.Types.Type)"

realLines :: [Text]
realLines = [uTys, noCoercion, coerced, filledLine, deferLine, defer2Line, unfilledOk, subTypeLine, instFunLine, instRetLine, "GHC.Types.Int{(w) tc 3u} ~nominal GHC.Types.Int{(w) tc 3u}", "A.T ~ A.T"]

-- | Words and punctuation that exercise every branch of the reader.
genSoup :: Gen Text
genSoup =
  T.concat
    <$> listOf
      ( elements
          [ "u_tys",
            " tclvl ",
            "0",
            " ~nominal ",
            "::",
            "Pair.hs:5:5-23",
            "(1,2)-(3,4)",
            "\n",
            " ",
            "found filled tyvar",
            " :-> ",
            "utype_defer",
            "utype_defer2",
            "x",
            ":",
            "-",
            "(",
            ")",
            "{tv}",
            " Nothing [",
            "] :: ",
            "GHC.Types.Int",
            "A.T",
            "B.T",
            "{(w) tc 3u}"
          ]
      )

spec :: Spec
spec = describe "Trace" $ do
  describe "locations" $ do
    it "reads a column range, with an exclusive end" $
      where' (parseRecord "Pair.hs:5:5-23:: x") `shouldBe` Just ("Pair.hs", "5:5", "5:24")
    it "reads a single column as width 1" $
      where' (parseRecord "Arg.hs:4:6::\n      {EpAnn}") `shouldBe` Just ("Arg.hs", "4:6", "4:7")
    it "reads a span over several lines" $
      where' (parseRecord "src/A.hs:(12,3)-(14,10):: u_tys yields no coercion") `shouldBe` Just ("src/A.hs", "12:3", "14:11")
    it "keeps a colon inside the path" $
      where' (parseRecord "C:/p/A.hs:7:1-3:: x") `shouldBe` Just ("C:/p/A.hs", "7:1", "7:4")
    it "gives no location to a message without a span" $
      where' (parseRecord "checkFamInstConsistency [Prelude]") `shouldBe` Nothing
    it "gives no location when the text before :: is not a span" $
      where' (parseRecord "lk1 x :: Type") `shouldBe` Nothing
    it "gives no location to a span that ends in column 0" $
      where' (parseRecord "A.hs:(5,1)-(6,0):: x") `shouldBe` Nothing
  describe "steps, read from lines GHC printed" $ do
    it "reads a unification" $
      Just (stepOf uTys)
        `shouldBe` (Unify 0 <$> mk "[] (a_ax0{tv} Nothing [tau:0] :: GHC.Types.Type)" <*> mk "[] GHC.Types.Int")
    it "reads the same unification whatever the line layout" $
      Just (recordStep (parseRecord "Arg.hs:4:6::\n  u_tys\n    tclvl 1\n    * ~nominal TYPE t_az7[tau:1]"))
        `shouldBe` (Unify 1 <$> mk "*" <*> mk "TYPE t_az7[tau:1]")
    it "reads a unification without evidence" $ stepOf noCoercion `shouldBe` UnifySucceeded
    it "reads a unification with evidence" $
      stepOf coerced `shouldBe` UnifyCoerced (T.drop (T.length "u_tys yields coercion: ") coerced)
    it "reads a type variable that was already filled" $
      Just (stepOf filledLine)
        `shouldBe` (TyVarFilled <$> mk "(a_ax0{tv} Nothing [tau:0] :: GHC.Types.Type)" <*> mk "GHC.Types.Int")
    it "reads a deferral with its role" $
      case stepOf deferLine of
        Deferral role detail -> (role, "In the second argument" `T.isInfixOf` detail) `shouldBe` ("nominal", True)
        other -> expectationFailure ("not a deferral: " <> show other)
    it "does not mistake the evidence record for a deferral" $
      case stepOf defer2Line of
        DeferralCoercion _ -> pure ()
        other -> expectationFailure ("not a deferral's evidence: " <> show other)
    it "reads whether GHC could fill a variable" $ do
      stepOf unfilledOk `shouldSatisfy` (\case TyVarUnify True _ -> True; _ -> False)
      stepOf "uUnfilledVar2 not ok" `shouldBe` TyVarUnify False ""
    it "reads a subtype check, actual first" $
      Just (stepOf subTypeLine)
        `shouldBe` (SubTypeCheck <$> mk "GHC.Internal.Base.String" <*> mk "(a_ax0{tv} Nothing [tau:0] :: GHC.Types.Type)")
    it "reads an instantiated function and its type" $
      case stepOf instFunLine of
        FunctionInstantiated f s -> (f, "forall" `T.isPrefixOf` unTypeText s, "fun_ctxt" `T.isInfixOf` unTypeText s) `shouldBe` ("Pair.pair", True, False)
        other -> expectationFailure ("not an instantiation: " <> show other)
    it "reads the result of an instantiation" $
      Just (stepOf instRetLine) `shouldBe` (FunctionResult <$> mk "[] (a_ax0{tv} Nothing [tau:0] :: GHC.Types.Type)")
    it "keeps a heading it does not read as unmodelled" $
      stepOf "tcWrapResult (a :: Type)" `shouldBe` Unmodelled "tcWrapResult"
    it "does not take a longer heading for one it knows" $
      stepOf "utype_defers x" `shouldBe` Unmodelled "utype_defers"
  describe "shapes it does not recognise" $ do
    it "reports a level that is not a number" $
      stepOf "u_tys tclvl x A ~nominal B" `shouldBe` Malformed "u_tys"
    it "reports a role it does not read" $
      stepOf "u_tys tclvl 0 A ~representational B" `shouldBe` Malformed "u_tys"
    it "reports a filled variable with no arrow" $
      stepOf "found filled tyvar a" `shouldBe` Malformed "found"
    it "reports extra text after a finished unification" $
      stepOf "u_tys yields no coercion at all" `shouldBe` Malformed "u_tys"
    it "counts malformed records in a trace" $
      fmap (fmap malformedCount) (run 10 (const True) ["A.hs:1:1:: u_tys tclvl x A ~nominal B", "A.hs:1:1:: u_tys yields no coercion", "A.hs:1:1:: found filled tyvar a"])
        `shouldBe` Just (Right 2)
  describe "parseRecord" $ do
    prop "is total and keeps the raw text" $
      forAll genSoup $ \t ->
        let r = parseRecord t in recordRaw r == t && not (null (show r))
  describe "the readable view" $ do
    it "removes the decorations of a type" $
      cleanType "u_tys tclvl 0 [] (a_ax0{tv} Nothing [tau:0] :: GHC.Types.Type) ~nominal [] GHC.Types.Int"
        `shouldBe` "u_tys tclvl 0 [] a_ax0[tau:0] ~nominal [] Int"
    it "removes the decorations of a name" $
      cleanType "GHC.Types.Int{(w) tc 3u} ~nominal GHC.Types.Int{(w) tc 3u}" `shouldBe` "Int ~nominal Int"
    it "keeps the qualifiers when two modules share a bare name" $
      cleanBlock ["A.T ~ B.T", "A.T"] `shouldBe` ["A.T ~ B.T", "A.T"]
    it "drops a qualifier that collides with nothing" $
      cleanType "A.T ~ A.T" `shouldBe` "T ~ T"
    it "leaves a shape it does not know as GHC printed it, and says so" $ do
      let t = "(co_axa{v} Just GHC.Types.Many [lid[CoVarId]] :: GHC.Prim.~# GHC.Types.Type GHC.Types.Type)"
      residue (cleanType t) `shouldBe` ["{v}"]
    it "removes the syntax-tree annotations and name tags of a deferral's context" $ do
      let t = "In the expression: ChainPair.pair{v rAeV} {EpAnn EpaSpan test/fixtures/ChainPair.hs:7:10-19 AnnListItem [] EpaComments []} (1 ({EpAnn EpaSpan test/fixtures/ChainPair.hs:7:11 NameAnnTrailing [] EpaComments []} GHC.Internal.Num.fromInteger{v 02A}) :: {EpAnn EpaSpan test/fixtures/ChainPair.hs:7:16-18 AnnListItem [] EpaComments []} {EpAnn EpaSpan test/fixtures/ChainPair.hs:7:16-18 AnnListItem [] EpaComments []} Int) {EpAnn EpaSpan test/fixtures/ChainPair.hs:7:21-23 AnnListItem [] EpaComments []} \"x\""
      -- Qualifiers are dropped from type names only, so these value names keep theirs.
      cleanType t `shouldBe` "In the expression: ChainPair.pair (1 (GHC.Internal.Num.fromInteger) :: Int) \"x\""
      residue (cleanType t) `shouldBe` []
    it "leaves an annotation block that is never closed as GHC printed it" $
      cleanType "x {EpAnn EpaSpan a.hs:1:1 [" `shouldBe` "x {EpAnn EpaSpan a.hs:1:1 ["
    it "reports no residue on a type it fully reads" $
      residue (cleanType uTys) `shouldBe` []
    prop "never makes the text longer" $
      forAll genSoup $
        \t -> T.length (cleanType t) <= T.length t
    prop "is idempotent on lines GHC printed" $
      forAll (listOf1 (elements realLines)) $ \ls ->
        let t = T.unwords ls in cleanType (cleanType t) == cleanType t
    prop "gives one entry per input" $
      forAll (listOf genSoup) $
        \ts -> length (cleanBlock ts) == length ts
    it "summarises records together" $
      cleanedSummaries [parseRecord ("Pair.hs:5:5-23:: " <> uTys), parseRecord ("Pair.hs:5:5-23:: " <> noCoercion)]
        `shouldBe` ["unify [] a_ax0[tau:0] with [] Int", "unification finished with no coercion"]
  describe "kind-level steps" $ do
    it "marks a unification of kind names" $
      map (stepKindLevel . stepOf) ["u_tys tclvl 0 GHC.Types.Type ~nominal GHC.Types.Type", "u_tys tclvl 0 GHC.Types.RuntimeRep ~nominal GHC.Types.RuntimeRep", "u_tys tclvl 0 GHC.Types.Type -> GHC.Types.Type ~nominal GHC.Types.Type -> GHC.Types.Type", "u_tys tclvl 0 GHC.Types.Many ~nominal GHC.Types.Many"]
        `shouldBe` [True, True, True, True]
    it "does not mark a unification of ordinary types" $
      map (stepKindLevel . stepOf) ["u_tys tclvl 0 GHC.Types.Int ~nominal GHC.Types.Int", uTys, "u_tys tclvl 1 * ~nominal TYPE t_az7[tau:1]", "found filled tyvar a :-> GHC.Types.Type"]
        `shouldBe` [False, False, False, False]
  describe "keeping a bounded number of records" $ do
    it "rejects a limit below 1" $ do
      mkRecordLimit 0 `shouldBe` Left (NonPositiveLimit "record limit" 0)
      renderDomainError (NonPositiveLimit "record limit" 0) `shouldSatisfy` T.isInfixOf "at least 1"
    prop "keeps the wanted records in order, up to the limit" $
      forAll ((,) <$> choose (1, 12) <*> listOf (elements ["A.hs:1:1:: u_tys yields no coercion", "B.hs:1:1:: x", "A.hs:2:2:: y"])) $ \(n, ms) ->
        let wanted r = fmap (\(f, _, _) -> f) (where' r) == Just "A.hs"
            expected = filter (\m -> "A.hs" `T.isPrefixOf` m) ms
         in fmap (fmap (map recordRaw . traceRecords)) (run n wanted ms)
              === Just (if length expected <= n then Right expected else Left (TraceLimitExceeded n))
    prop "counts every message offered" $
      forAll (listOf (elements ["A.hs:1:1:: x", "y"])) $ \ms ->
        fmap (fmap traceSeen) (run 10 (const False) ms) === Just (Right (length ms))
    it "gives an error, not a truncated trace, past the limit" $
      fmap (fmap traceRecords) (run 3 (const True) (replicate 4 "A.hs:1:1:: x"))
        `shouldBe` Just (Left (TraceLimitExceeded 3))
    it "stays within its limit over a very long trace" $
      fmap (fmap traceSeen) (run 10 (const True) (replicate 100000 "A.hs:1:1:: x"))
        `shouldBe` Just (Left (TraceLimitExceeded 10))
    it "reads a very long trace when few records are wanted" $
      fmap (fmap traceSeen) (run 10 (const False) (replicate 100000 "A.hs:1:1:: x"))
        `shouldBe` Just (Right 100000)
    it "says in words what went wrong" $
      renderTraceError (TraceLimitExceeded 3) `shouldSatisfy` T.isInfixOf "more than 3"
