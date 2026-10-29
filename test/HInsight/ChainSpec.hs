module HInsight.ChainSpec (spec) where

import Data.Maybe (listToMaybe)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import HInsight.Chain
import HInsight.Source (Span, spanContains)
import HInsight.Trace (Located (..), TraceRecord (..), locatedIn, parseRecord)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

-- | The span a trace location reads as, taken from the parser under test.
spanOf :: Text -> Maybe Span
spanOf t = locatedSpan <$> recordLocation (parseRecord (t <> ":: x"))

-- | Run a check with the span of the binding @r@ and of the error in @Pair.hs@.
withPair :: (Span -> Span -> Expectation) -> Expectation
withPair check = case (spanOf "Pair.hs:5:1-23", spanOf "Pair.hs:5:21-23") of
  (Just binding, Just err) -> check binding err
  _ -> expectationFailure "the spans of the fixture could not be read"

pairChains :: ChainView -> [Text] -> (Chains -> Expectation) -> Expectation
pairChains view extra check = withPair $ \binding err ->
  check (buildChains view [binding] [err] (map parseRecord (pairTrace <> extra)))

summaries :: Chains -> [[Text]]
summaries = map (map chainStepSummary . chainSteps) . chainsFound

-- | Records from a small trace in which a variable links two others.
linkTrace :: [Text]
linkTrace =
  [ "A.hs:1:1-2:: u_tys tclvl 0 v2_b[tau:0] ~nominal Bool",
    "A.hs:1:3-4:: u_tys tclvl 0 v1_a[tau:0] ~nominal v2_b[tau:0]",
    "A.hs:1:7-8:: u_tys tclvl 0 v9_z[tau:0] ~nominal Char",
    "A.hs:1:5-6:: u_tys tclvl 0 v1_a[tau:0] ~nominal Int",
    "A.hs:1:5-6:: utype_defer nominal Int Bool"
  ]

spec :: Spec
spec = describe "Chain" $ do
  describe "locatedIn, the filter the capture hook uses" $ do
    it "keeps a record inside a binding of the file" $
      withPair $ \binding _ ->
        locatedIn "Pair.hs" [binding] (parseRecord "Pair.hs:5:21-23:: x") `shouldBe` True
    it "reads ./Pair.hs and Pair.hs as the same file" $
      withPair $ \binding _ ->
        locatedIn "./Pair.hs" [binding] (parseRecord "Pair.hs:5:21-23:: x") `shouldBe` True
    it "drops a record of another file" $
      withPair $ \binding _ ->
        locatedIn "Pair.hs" [binding] (parseRecord "Other.hs:5:21-23:: x") `shouldBe` False
    it "drops a record outside every binding" $
      withPair $ \binding _ ->
        locatedIn "Pair.hs" [binding] (parseRecord "Pair.hs:1:1-3:: x") `shouldBe` False
    it "drops a record with no location" $
      withPair $ \binding _ ->
        locatedIn "Pair.hs" [binding] (parseRecord "no location here") `shouldBe` False
    it "keeps nothing when the file has no bindings" $
      locatedIn "Pair.hs" [] (parseRecord "Pair.hs:5:21-23:: x") `shouldBe` False
  describe "typeVariables" $ do
    it "reads unification and rigid variables, as GHC prints them" $
      typeVariables "u_tys tclvl 0 [] (a_ax0{tv} Nothing [tau:0] :: GHC.Types.Type) ~nominal (a Nothing [sk:2] :: GHC.Types.Type)"
        `shouldBe` S.fromList ["a_ax0[tau:0]", "a[sk:2]"]
    it "finds a variable written inside a list type" $
      typeVariables "[a_ax0[tau:0]]" `shouldBe` S.fromList ["a_ax0[tau:0]"]
    it "does not take the list type or a literal list for a variable" $
      typeVariables "[] [1,2] Int" `shouldBe` S.empty
  describe "the Pair program, from a real GHC 9.10.3 trace" $ do
    it "following the variable gives the golden sequence, ending at the deferral" $
      pairChains FollowingVariables [] $ \cs -> do
        map (take 10) (summaries cs)
          `shouldBe` [ [ "instantiated result [] a_ax0[tau:0]",
                         "unify [] a_ax0[tau:0] with [] Int",
                         "unify a_ax0[tau:0] with Int",
                         "GHC could fill a variable: a_ax0[tau:0] :: Type Int :: Type True <*>_N",
                         "unify Int with a_ax0[tau:0]",
                         "variable a_ax0[tau:0] was already Int",
                         "check actual String against expected a_ax0[tau:0]",
                         "unify String with a_ax0[tau:0]",
                         "variable a_ax0[tau:0] was already Int",
                         "unify String with Int"
                       ]
                     ]
        map (length . chainSteps) (chainsFound cs) `shouldBe` [11]
        chainsMissing cs `shouldBe` 0
    it "ends at a deferral whose span is the error's" $
      withPair $ \binding err ->
        case chainsFound (buildChains FollowingVariables [binding] [err] (map parseRecord pairTrace)) of
          [c] -> do
            chainSpan c `shouldBe` err
            fmap chainStepSpan (lastOf (chainSteps c)) `shouldBe` Just err
            fmap chainStepSummary (lastOf (chainSteps c)) `shouldSatisfy` maybe False (T.isPrefixOf "GHC deferred the nominal equality")
          other -> expectationFailure ("expected one chain, got " <> show (length other))
    it "every record of the binding gives all interpreted steps, with kind-level ones marked" $
      pairChains WholeBinding [] $ \cs -> do
        map (length . chainSteps) (chainsFound cs) `shouldBe` [31]
        map (length . filter chainStepKindLevel . chainSteps) (chainsFound cs) `shouldBe` [2]
    it "leaves out the steps that do not share a variable, without losing the deferral" $
      withPair $ \binding err -> do
        let steps v = concatMap chainSteps (chainsFound (buildChains v [binding] [err] (map parseRecord pairTrace)))
        all (`elem` steps WholeBinding) (steps FollowingVariables) `shouldBe` True
        length (steps FollowingVariables) < length (steps WholeBinding) `shouldBe` True
    it "keeps the raw record of every step" $
      pairChains FollowingVariables [] $ \cs ->
        all (all (T.isInfixOf "Pair.hs:5:" . chainStepRaw) . chainSteps) (chainsFound cs) `shouldBe` True
  describe "selection" $ do
    it "follows a variable through another record" $
      case (spanOf "A.hs:1:1-8", spanOf "A.hs:1:5-6") of
        (Just binding, Just err) ->
          map (map chainStepSummary . chainSteps) (chainsFound (buildChains FollowingVariables [binding] [err] (map parseRecord linkTrace)))
            `shouldBe` [ [ "unify v2_b[tau:0] with Bool",
                           "unify v1_a[tau:0] with v2_b[tau:0]",
                           "unify v1_a[tau:0] with Int",
                           "GHC deferred the nominal equality: Int Bool"
                         ]
                       ]
        _ -> expectationFailure "the spans of the fixture could not be read"
    it "has no chain for an error with no deferral at its span" $
      withPair $ \binding _ ->
        case spanOf "Pair.hs:5:5-9" of
          Just other -> buildChains FollowingVariables [binding] [other] (map parseRecord pairTrace) `shouldBe` Chains [] 1
          Nothing -> expectationFailure "span"
    it "has no chain for an error outside every binding" $
      withPair $
        \_ err -> buildChains FollowingVariables [] [err] (map parseRecord pairTrace) `shouldBe` Chains [] 1
    it "ignores records outside the binding of the error" $
      pairChains FollowingVariables ["Pair.hs:9:1-5:: utype_defer nominal A B"] $ \cs ->
        map (length . chainSteps) (chainsFound cs) `shouldBe` [11]
    it "does not count an uninterpreted record that came after the deferral" $
      pairChains WholeBinding ["Pair.hs:5:30-31:: tcWrapResult x"] $ \cs ->
        map chainOmitted (chainsFound cs) `shouldBe` [0]
    it "counts an uninterpreted record that came before the deferral" $
      withPair $ \binding err -> do
        let unread = ["Pair.hs:5:21-23:: tcWrapResult x"]
            records = map parseRecord (take 28 pairTrace <> unread <> drop 28 pairTrace)
        map chainOmitted (chainsFound (buildChains WholeBinding [binding] [err] records)) `shouldBe` [1]
    it "uses the first deferral when there are two at the span" $
      withPair $ \binding err ->
        map (length . chainSteps) (chainsFound (buildChains WholeBinding [binding] [err] (map parseRecord (pairTrace <> ["Pair.hs:5:21-23:: utype_defer nominal X Y"]))))
          `shouldBe` [31]
  describe "properties" $ do
    prop "every chain ends at a deferral at its span, and every error is a chain or counted" $
      forAll (sublistOf pairTrace) $ \kept ->
        forAll (elements [FollowingVariables, WholeBinding]) $ \view ->
          case (spanOf "Pair.hs:5:1-23", spanOf "Pair.hs:5:21-23") of
            (Just binding, Just err) ->
              let cs = buildChains view [binding] [err] (map parseRecord kept)
               in length (chainsFound cs)
                    + chainsMissing cs
                    === 1
                    .&&. conjoin
                      [ fmap chainStepSpan (lastOf (chainSteps c))
                          === Just err
                          .&&. counterexample "last step" (maybe False (T.isPrefixOf "GHC deferred" . chainStepSummary) (lastOf (chainSteps c)))
                      | c <- chainsFound cs
                      ]
            _ -> counterexample "spans" False
    prop "is total on any text" $
      forAll (listOf (elements pairTrace)) $ \ts ->
        case spanOf "Pair.hs:5:1-23" of
          Just binding -> not (null (show (buildChains FollowingVariables [binding] [binding] (map parseRecord ts))))
          Nothing -> False
  describe "bindings that contain errors" $ do
    let bindings = mapM spanOf ["A.hs:1:1-10", "A.hs:3:1-10", "A.hs:5:1-10"]
        errors = mapM spanOf ["A.hs:3:5-6", "A.hs:5:2-3", "A.hs:9:1-2"]
    it "keeps the bindings that contain an error, in order" $
      case (bindings, errors) of
        (Just bs, Just es) -> bindingsWithErrors es bs `shouldBe` drop 1 bs
        _ -> expectationFailure "spans did not parse"
    it "keeps none when there is no error" $
      case bindings of
        Just bs -> bindingsWithErrors [] bs `shouldBe` []
        Nothing -> expectationFailure "spans did not parse"
    prop "keeps exactly the bindings that contain a chosen error" $
      case (bindings, errors) of
        (Just bs, Just es) -> forAll (sublistOf es) $ \chosen ->
          let kept = bindingsWithErrors chosen bs
           in conjoin [(b `elem` kept) === any (b `spanContains`) chosen | b <- bs]
        _ -> counterexample "spans did not parse" False
  describe "rendering" $ do
    it "numbers the steps, shows positions and marks kind-level steps" $
      pairChains WholeBinding [] $ \cs -> do
        let out = renderChains cs
        out `shouldSatisfy` T.isInfixOf "what GHC logged before it gave up at 5:21"
        out `shouldSatisfy` T.isInfixOf "  1. 5:5  instantiate Pair.pair"
        out `shouldSatisfy` T.isInfixOf "[kind level]"
    it "says how many errors have no chain" $
      renderChains (Chains [] 2) `shouldBe` "2 error(s) have no chain: GHC logged no deferral at their span\n"
    it "says so when there was nothing to explain" $
      renderChains emptyChains `shouldBe` "no chains: GHC reported no errors\n"
  where
    lastOf :: [a] -> Maybe a
    lastOf = listToMaybe . reverse

pairTrace :: [Text]
pairTrace =
  [ "Pair.hs:5:5-23:: tcInstFun tc_fun Pair.pair fun_sigma forall (a Nothing [sk:2] :: GHC.Types.Type). (a Nothing [sk:2] :: GHC.Types.Type) -> (a Nothing [sk:2] :: GHC.Types.Type) -> [] (a Nothing [sk:2] :: GHC.Types.Type) fun_ctxt VA",
    "Pair.hs:5:5-23:: tcInstFun:ret [] (a_ax0{tv} Nothing [tau:0] :: GHC.Types.Type)",
    "Pair.hs:5:5-23:: u_tys tclvl 0 [] (a_ax0{tv} Nothing [tau:0] :: GHC.Types.Type) ~nominal [] GHC.Types.Int",
    "Pair.hs:5:5-23:: u_tys tclvl 0 (a_ax0{tv} Nothing [tau:0] :: GHC.Types.Type) ~nominal GHC.Types.Int",
    "Pair.hs:5:5-23:: u_tys tclvl 0 GHC.Types.Type ~nominal GHC.Types.Type",
    "Pair.hs:5:5-23:: u_tys yields no coercion",
    "Pair.hs:5:5-23:: uUnfilledVar2 ok (a_ax0{tv} Nothing [tau:0] :: GHC.Types.Type) :: GHC.Types.Type GHC.Types.Int :: GHC.Types.Type True <*>_N",
    "Pair.hs:5:5-23:: u_tys yields no coercion",
    "Pair.hs:5:5-23:: u_tys yields no coercion",
    "Pair.hs:5:16-18:: u_tys tclvl 1 GHC.Types.Type ~nominal GHC.Prim.TYPE (t_ax3{tv} Nothing [tau:1] :: GHC.Types.RuntimeRep)",
    "Pair.hs:5:16-18:: u_tys tclvl 1 GHC.Types.LiftedRep ~nominal (t_ax3{tv} Nothing [tau:1] :: GHC.Types.RuntimeRep)",
    "Pair.hs:5:16-18:: u_tys tclvl 1 GHC.Types.RuntimeRep ~nominal GHC.Types.RuntimeRep",
    "Pair.hs:5:16-18:: u_tys yields no coercion",
    "Pair.hs:5:16-18:: uUnfilledVar2 ok (t_ax3{tv} Nothing [tau:1] :: GHC.Types.RuntimeRep) :: GHC.Types.RuntimeRep GHC.Types.LiftedRep :: GHC.Types.RuntimeRep True <GHC.Types.RuntimeRep>_N",
    "Pair.hs:5:16-18:: u_tys yields no coercion",
    "Pair.hs:5:16-18:: u_tys yields no coercion",
    "Pair.hs:5:16-18:: tc_sub_type (general case) ty_actual   = GHC.Types.Int ty_expected = GHC.Types.Int",
    "Pair.hs:5:16-18:: u_tys tclvl 0 GHC.Types.Int ~nominal GHC.Types.Int",
    "Pair.hs:5:16-18:: u_tys yields no coercion",
    "Pair.hs:5:11-18:: tcInstFun tc_fun 1 (1) :: {EpAnn EpaSpan Pair.hs:5:16-18 AnnListItem [] EpaComments []} {EpAnn EpaSpan Pair.hs:5:16-18 AnnListItem [] EpaComments []} GHC.Types.Int fun_sigma GHC.Types.Int fun_ctxt VACall 0 1 ({Ep",
    "Pair.hs:5:11-18:: tcInstFun:ret GHC.Types.Int",
    "Pair.hs:5:11-18:: u_tys tclvl 0 GHC.Types.Int ~nominal (a_ax0{tv} Nothing [tau:0] :: GHC.Types.Type)",
    "Pair.hs:5:11-18:: found filled tyvar (a_ax0{tv} Nothing [tau:0] :: GHC.Types.Type) :-> GHC.Types.Int",
    "Pair.hs:5:11-18:: u_tys tclvl 0 GHC.Types.Int ~nominal GHC.Types.Int",
    "Pair.hs:5:11-18:: u_tys yields no coercion",
    "Pair.hs:5:11-18:: u_tys yields no coercion",
    "Pair.hs:5:21-23:: tc_sub_type (general case) ty_actual   = GHC.Internal.Base.String ty_expected = (a_ax0{tv} Nothing [tau:0] :: GHC.Types.Type)",
    "Pair.hs:5:21-23:: u_tys tclvl 0 GHC.Internal.Base.String ~nominal (a_ax0{tv} Nothing [tau:0] :: GHC.Types.Type)",
    "Pair.hs:5:21-23:: found filled tyvar (a_ax0{tv} Nothing [tau:0] :: GHC.Types.Type) :-> GHC.Types.Int",
    "Pair.hs:5:21-23:: u_tys tclvl 0 GHC.Internal.Base.String ~nominal GHC.Types.Int",
    "Pair.hs:5:21-23:: utype_defer nominal GHC.Internal.Base.String GHC.Types.Int In the second argument of ‘Pair.pair’, namely ‘\"x\"’ In the expression: Pair.pair {EpAnn EpaSpan Pair.hs:5:10-19 AnnListItem [] EpaComments []} (1",
    "Pair.hs:5:21-23:: utype_defer2 {(co_axa{v} Just GHC.Types.Many [lid[CoVarId]] :: GHC.Prim.~# GHC.Types.Type GHC.Types.Type GHC.Internal.Base.String GHC.Types.Int)}",
    "Pair.hs:5:21-23:: u_tys yields coercion: {(co_axa{v} Just GHC.Types.Many [lid[CoVarId]] :: GHC.Prim.~# GHC.Types.Type GHC.Types.Type GHC.Internal.Base.String GHC.Types.Int)}",
    "Pair.hs:5:21-23:: u_tys yields coercion: {(co_axa{v} Just GHC.Types.Many [lid[CoVarId]] :: GHC.Prim.~# GHC.Types.Type GHC.Types.Type GHC.Internal.Base.String GHC.Types.Int)}"
  ]
