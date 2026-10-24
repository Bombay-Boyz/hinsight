-- |
-- Module      : HInsight.Chain
-- Description : What GHC logged on the way to an error, in the order it logged it.
--
-- A 'Chain' is not the cause of an error. It is the part of GHC's typechecker
-- trace that leads up to the point where GHC gave up on an equality (a
-- deferral) at the error's own span, taken from the records read by
-- "HInsight.Trace". Two views of the same records are offered, both
-- deterministic and neither inferring anything:
--
-- * 'FollowingVariables': the records GHC logged at the error's span, and
--   every earlier record in the binding that shares a type variable with them,
--   repeated until nothing more is added.
-- * 'WholeBinding': every record GHC logged in the binding before the deferral.
--
-- Records this package does not interpret are never shown; they are counted in
-- 'chainOmitted' so that the omission is visible. Steps about kinds are marked,
-- not hidden. An error with no deferral at its span has no chain and is
-- counted in 'chainsMissing'.
module HInsight.Chain
  ( ChainView (..),
    ChainStep,
    chainStepSpan,
    chainStepSummary,
    chainStepRaw,
    chainStepKindLevel,
    Chain,
    chainSpan,
    chainView,
    chainSteps,
    chainOmitted,
    Chains (..),
    emptyChains,
    buildChains,
    selectChain,
    typeVariables,
    renderChain,
    renderChains,
  )
where

import Data.Char (isAlphaNum)
import Data.List (findIndex)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import HInsight.Source (Span, renderPosition, spanContains, spanStart)
import HInsight.Trace
  ( Located (..),
    Step (..),
    TraceRecord (..),
    cleanedSummaries,
    recordText,
    stepKindLevel,
  )
import HInsight.Trace.Clean (cleanType)

-- | Which records of the binding a chain shows.
data ChainView
  = -- | The records at the error's span and those linked to them by a type
    -- variable.
    FollowingVariables
  | -- | Every record in the binding up to the deferral.
    WholeBinding
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | One record of a chain. Invariant: it was read from a record that has a
-- location.
data ChainStep = ChainStep
  { chainStepSpan :: !Span,
    -- | What the record says, with GHC's debug decorations removed.
    chainStepSummary :: !Text,
    -- | The record as GHC logged it.
    chainStepRaw :: !Text,
    -- | Whether the step is certainly between kinds, judged by printed form.
    chainStepKindLevel :: !Bool
  }
  deriving stock (Eq, Show)

-- | The records leading to a deferral at an error's span. Invariants: there is
-- at least one step, and the last is the deferral.
data Chain = Chain
  { chainSpan :: !Span,
    chainView :: !ChainView,
    chainSteps :: ![ChainStep],
    -- | Records that would have been shown but are not interpreted.
    chainOmitted :: !Int
  }
  deriving stock (Eq, Show)

-- | The chains found for a file, and how many errors have none.
data Chains = Chains
  { chainsFound :: ![Chain],
    -- | Errors GHC reported that have no chain: no deferral was logged at
    -- their span, or the trace was not available.
    chainsMissing :: !Int
  }
  deriving stock (Eq, Show)

-- | No chains and no missing errors.
emptyChains :: Chains
emptyChains = Chains [] 0

-- | The chains for the errors at these spans. The binding spans say which
-- records belong to which error: those that lie inside the first binding that
-- contains the error.
buildChains :: ChainView -> [Span] -> [Span] -> [TraceRecord] -> Chains
buildChains view bindings errors records = Chains found (length errors - length found)
  where
    found :: [Chain]
    found = [chain | e <- errors, Just chain <- [chainAt e]]
    chainAt :: Span -> Maybe Chain
    chainAt e = do
      binding <- firstContaining bindings e
      selectChain view e (filter (inside binding) records)
    firstContaining :: [Span] -> Span -> Maybe Span
    firstContaining spans e = case filter (`spanContains` e) spans of
      b : _ -> Just b
      [] -> Nothing
    inside :: Span -> TraceRecord -> Bool
    inside binding r = maybe False ((binding `spanContains`) . locatedSpan) (recordLocation r)

-- | The chain for the error at a span, from the records of its binding in the
-- order GHC logged them. 'Nothing' when no deferral was logged at that span.
selectChain :: ChainView -> Span -> [TraceRecord] -> Maybe Chain
selectChain view errorSpan records = do
  deferral <- findIndex (isDeferralAt errorSpan) located
  let upTo = take (deferral + 1) located
      chosen = case view of
        WholeBinding -> upTo
        FollowingVariables -> linked errorSpan upTo
      shown = filter (interpreted . snd) chosen
      steps = zipWith step shown (cleanedSummaries (map snd shown))
  case steps of
    [] -> Nothing
    _ -> Just (Chain errorSpan view steps (length chosen - length shown))
  where
    located :: [(Located, TraceRecord)]
    located = [(l, r) | r <- records, Just l <- [recordLocation r]]

    step :: (Located, TraceRecord) -> Text -> ChainStep
    step (l, r) summary = ChainStep (locatedSpan l) summary (recordRaw r) (stepKindLevel (recordStep r))

isDeferralAt :: Span -> (Located, TraceRecord) -> Bool
isDeferralAt errorSpan (l, r) = locatedSpan l == errorSpan && isDeferral (recordStep r)
  where
    isDeferral :: Step -> Bool
    isDeferral = \case
      Deferral _ _ -> True
      _ -> False

interpreted :: TraceRecord -> Bool
interpreted r = case recordStep r of
  Unmodelled _ -> False
  _ -> True

-- | The records at the error's span, and those that share a type variable with
-- them, directly or through other records, repeated until nothing is added.
-- The loop cannot run more times than there are records.
linked :: Span -> [(Located, TraceRecord)] -> [(Located, TraceRecord)]
linked errorSpan records = [rec | (rec, vs) <- withVariables, atError rec || belongs vs final]
  where
    withVariables :: [((Located, TraceRecord), Set Text)]
    withVariables = [(rec, typeVariables (recordText (snd rec))) | rec <- records]

    atError :: (Located, TraceRecord) -> Bool
    atError = (== errorSpan) . locatedSpan . fst

    seeds :: Set Text
    seeds = S.unions [vs | (rec, vs) <- withVariables, atError rec]

    belongs :: Set Text -> Set Text -> Bool
    belongs vs shared = not (S.disjoint vs shared)

    final :: Set Text
    final = grow (length records) seeds

    grow :: Int -> Set Text -> Set Text
    grow fuel shared
      | fuel <= 0 || next == shared = shared
      | otherwise = grow (fuel - 1) next
      where
        next :: Set Text
        next = S.unions (shared : [vs | (rec, vs) <- withVariables, atError rec || belongs vs shared])

-- | The type variables in a text GHC printed under @-dppr-debug@, as they read
-- after cleaning: a name followed by its level, such as @a_ax0[tau:0]@ (a
-- unification variable) or @a[sk:2]@ (a rigid one). Two variables that print
-- the same are taken to be the same.
typeVariables :: Text -> Set Text
typeVariables = S.fromList . variablesIn . cleanType
  where
    variablesIn :: Text -> [Text]
    variablesIn t = case T.breakOn "[" t of
      (_, rest) | T.null rest -> []
      (before, rest) ->
        let name = T.takeWhileEnd isIdentChar before
            (info, _) = T.breakOn "]" (T.drop 1 rest)
            here = [name <> "[" <> info <> "]" | not (T.null name), isLevel info]
         in here <> variablesIn (T.drop 1 rest)

    isLevel :: Text -> Bool
    isLevel info = "tau:" `T.isPrefixOf` info || "sk:" `T.isPrefixOf` info

    isIdentChar :: Char -> Bool
    isIdentChar c = isAlphaNum c || c == '_' || c == '\''

-- | Render a chain as plain lines: a heading with the error's position, then
-- one numbered line per step in the order GHC logged them.
renderChain :: Chain -> Text
renderChain c = T.unlines (heading : zipWith line [1 :: Int ..] (chainSteps c) <> omitted)
  where
    heading :: Text
    heading =
      "what GHC logged before it gave up at "
        <> renderPosition (spanStart (chainSpan c))
        <> " ("
        <> viewName (chainView c)
        <> "):"
    line :: Int -> ChainStep -> Text
    line n s =
      "  "
        <> T.pack (show n)
        <> ". "
        <> renderPosition (spanStart (chainStepSpan s))
        <> "  "
        <> chainStepSummary s
        <> (if chainStepKindLevel s then "  [kind level]" else "")
    omitted :: [Text]
    omitted =
      [ "  (" <> T.pack (show (chainOmitted c)) <> " records GHC logged here are not interpreted)"
      | chainOmitted c > 0
      ]
    viewName :: ChainView -> Text
    viewName = \case
      FollowingVariables -> "the records at that span and those that share a type variable with them"
      WholeBinding -> "every record in the binding"

-- | Render every chain, then say how many errors have none.
renderChains :: Chains -> Text
renderChains cs = T.concat (map renderChain (chainsFound cs)) <> footer
  where
    footer :: Text
    footer
      | chainsMissing cs > 0 =
          T.pack (show (chainsMissing cs))
            <> " error(s) have no chain: GHC logged no deferral at their span\n"
      | null (chainsFound cs) = "no chains: GHC reported no errors\n"
      | otherwise = ""
