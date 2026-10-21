{-# LANGUAGE GADTs #-}

-- |
-- Module      : HInsight.Tadka.Support
-- Description : Values for the specs, built through the smart constructors.
module HInsight.Tadka.Support
  ( ok,
    sourceOf,
    span',
    ty,
    ident,
    mismatchAt,
    signedAt,
    holeAt,
    instanceAt,
    suggestedAt,
    pipelineAt,
    analysisOf,
    narrate,
    describeJson,
  )
where

import Data.Aeson qualified as Aeson
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import HInsight.Analysis (Analysis (..), emptyAnalysis)
import HInsight.Context (Context (..))
import HInsight.Explanation (Explanation (..), mkDivergence)
import HInsight.Flow (FlowKind (..), Pipeline, Stage (..), mkPipeline, mkStageText)
import HInsight.Hole (HoleFit (..), HoleReport (..), Locality (..), mkRefinementLevel)
import HInsight.Identifier (Identifier, mkIdentifier)
import HInsight.Instance (UnresolvedInstance (..), mkInstanceText)
import HInsight.Source (Span, mkColumn, mkLine, mkPosition, mkSpan)
import HInsight.Suggestion (HintKind, Suggested (..), mkSuggestion)
import HInsight.Tadka (Finding, namedSource)
import HInsight.TypeText (TypeText, mkTypeText)
import Tadka qualified as Td

ok :: (Show e) => Either e a -> IO a
ok = either (fail . show) pure

-- | A named source with this text.
sourceOf :: Text -> IO Td.NamedSource
sourceOf = ok . namedSource "Sample.hs"

-- | A span from (line, column) to (line, column), end exclusive.
span' :: (Int, Int) -> (Int, Int) -> IO Span
span' (l1, c1) (l2, c2) = do
  start <- mkPosition <$> ok (mkLine l1) <*> ok (mkColumn c1)
  end <- mkPosition <$> ok (mkLine l2) <*> ok (mkColumn c2)
  ok (mkSpan start end)

ty :: Text -> IO TypeText
ty = ok . mkTypeText

ident :: Text -> IO Identifier
ident = ok . mkIdentifier

-- | A mismatch of expected against actual at a span, in this context. The
-- divergence is the pair of types GHC could not match, when it names one.
mismatchAt :: Span -> Text -> Text -> Maybe (Text, Text) -> Context -> IO Explanation
mismatchAt sp expected actual divergence ctx = do
  e <- ty expected
  a <- ty actual
  d <- traverse (\(l, r) -> do l' <- ty l; r' <- ty r; ok (mkDivergence l' r')) divergence
  pure (Explanation sp e a d ctx)

-- | The context of a binding with a signature.
signedAt :: Text -> Span -> Text -> IO Context
signedAt name sp declared = InSignedBinding <$> ident name <*> pure sp <*> ty declared

holeAt :: Span -> Text -> [(Text, Text, Int, Locality)] -> IO HoleReport
holeAt sp wanted fits = do
  name <- ident "_"
  t <- ty wanted
  fs <- traverse mkFit fits
  pure (HoleReport name sp t fs)
  where
    mkFit :: (Text, Text, Int, Locality) -> IO HoleFit
    mkFit (n, t, level, locality) =
      HoleFit <$> ident n <*> ty t <*> ok (mkRefinementLevel level) <*> pure locality <*> pure []

-- | An unresolved constraint at a span, with the instances GHC lists for it.
instanceAt :: Span -> Text -> [Text] -> Context -> IO UnresolvedInstance
instanceAt sp constraint potential ctx = do
  c <- ty constraint
  ps <- traverse (ok . mkInstanceText) potential
  pure (UnresolvedInstance sp c ps ctx)

-- | The hints GHC gave for an error at a span; there is at least one.
suggestedAt :: Span -> (HintKind, Text) -> [(HintKind, Text)] -> IO Suggested
suggestedAt sp first rest = do
  hints <- traverse (ok . uncurry mkSuggestion) (first :| rest)
  pure (Suggested sp hints)

pipelineAt :: Span -> FlowKind -> [(Text, Text)] -> Text -> Context -> IO Pipeline
pipelineAt sp kind stages whole ctx = do
  ss <- traverse (\(n, t) -> Stage <$> ok (mkStageText n) <*> ty t) stages
  w <- ty whole
  ok (mkPipeline sp kind ss w ctx)

analysisOf :: [Explanation] -> [HoleReport] -> [Pipeline] -> Analysis
analysisOf ms hs ps = emptyAnalysis {analysisMismatches = ms, analysisHoles = hs, analysisFlows = ps}

-- | The narratable rendering of a finding.
narrate :: Finding -> Text
narrate f = case Td.selectRenderer (Td.withTarget Td.TNarratable Td.defaultConfig) of
  Td.SomeRenderer r@(Td.Narratable _) -> Td.render r f
  Td.SomeRenderer _ -> ""

-- | Whether the JSON rendering of a finding is a JSON object.
describeJson :: Finding -> Bool
describeJson f = case Td.selectRenderer (Td.withTarget Td.TJson Td.defaultConfig) of
  Td.SomeRenderer r@(Td.Json _) -> case Td.render r f of
    Aeson.Object _ -> True
    _ -> False
  Td.SomeRenderer _ -> False
