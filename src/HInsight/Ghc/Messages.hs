-- |
-- Module      : HInsight.Ghc.Messages
-- Description : Explain GHC's type-mismatch diagnostics.
--
-- Works on the structured diagnostics GHC 9.10 puts in a 'SourceError', not on
-- rendered text. Only mismatches that carry GHC's expected/actual information
-- are explained, whether GHC reports them as a plain mismatch or as one that
-- involves a rigid type variable. A class constraint GHC could not solve is
-- reported with the instances GHC lists for it. Every other error is counted,
-- never dropped silently. A real typed hole is reported through the hole-fit
-- plugin instead, and is tallied here so the caller can check that the plugin
-- saw it.
--
-- The hints GHC attaches to a diagnostic are collected for every kind of
-- diagnostic, besides how it is classified. A hint on a diagnostic that has no
-- source location is not reported, since there is nothing to anchor it to.
module HInsight.Ghc.Messages
  ( Extracted (..),
    extractMessages,
  )
where

import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import GHC.Core.InstEnv (ClsInst)
import GHC.Core.Type (Type)
import GHC.Driver.Errors.Ppr ()
import GHC.Driver.Errors.Types (GhcMessage (..))
import GHC.Tc.Errors.Types
  ( HoleError (..),
    MismatchEA (..),
    MismatchMsg (..),
    SolverReportWithCtxt (..),
    TcRnMessage (..),
    TcRnMessageDetailed (..),
    TcSolverReportMsg (..),
    errorItemPred,
  )
import GHC.Types.Error (Diagnostic (diagnosticHints), MsgEnvelope (..))
import GHC.Types.Hint (GhcHint (..))
import GHC.Types.Hint.Ppr ()
import GHC.Types.SrcLoc (RealSrcSpan, SrcSpan (..))
import HInsight.Context (Context)
import HInsight.Error (InsightError)
import HInsight.Explanation
  ( Divergence,
    Explanation (..),
    mkDivergence,
  )
import HInsight.Ghc.Bindings (Binding, contextAt)
import HInsight.Ghc.Convert (convertSpan, convertText, fromDomain, renderUser)
import HInsight.Instance (UnresolvedInstance (..), mkInstanceText)
import HInsight.Source (Span)
import HInsight.Suggestion (HintKind (..), Suggested (..), Suggestion, mkSuggestion)
import HInsight.TypeText (TypeText)

-- | The explained mismatches, the unresolved class constraints, GHC's hints,
-- the number of errors left unexplained, and the number of typed-hole errors
-- GHC reported (which the hole-fit plugin is expected to have seen).
data Extracted = Extracted
  { extractedMismatches :: ![Explanation],
    extractedInstances :: ![UnresolvedInstance],
    extractedSuggestions :: ![Suggested],
    extractedUnexplained :: !Int,
    extractedHoleDiagnostics :: !Int
  }
  deriving stock (Eq, Show)

-- | What became of one diagnostic.
data Classified
  = Explained Explanation
  | -- | A class constraint GHC could not solve.
    Unresolved UnresolvedInstance
  | -- | A typed hole or wildcard: its fits come through the hole-fit plugin.
    HoleDiagnostic
  | Unexplained

-- | The pieces of a mismatch GHC reported with an expected and an actual type.
data MismatchParts = MismatchParts
  { partsExpected :: Type,
    partsActual :: Type,
    -- | The two sub-types at which the mismatch was detected.
    partsLeft :: Type,
    partsRight :: Type
  }

-- | Classify and explain a list of error diagnostics from one file.
extractMessages :: FilePath -> [Binding] -> [MsgEnvelope GhcMessage] -> Either InsightError Extracted
extractMessages file bindings envs =
  tally
    <$> traverse (classify file bindings) envs
    <*> traverse (suggestionsOf file) envs

-- | Count by outcome. List comprehensions with a refutable pattern are the
-- filter-and-map here; 'length' is strict, so nothing accumulates lazily.
tally :: [Classified] -> [Maybe Suggested] -> Extracted
tally cs hints =
  Extracted
    { extractedMismatches = [e | Explained e <- cs],
      extractedInstances = [u | Unresolved u <- cs],
      extractedSuggestions = catMaybes hints,
      extractedUnexplained = length [() | Unexplained <- cs],
      extractedHoleDiagnostics = length [() | HoleDiagnostic <- cs]
    }

classify :: FilePath -> [Binding] -> MsgEnvelope GhcMessage -> Either InsightError Classified
classify file bindings env = case typecheckerMessage (errMsgDiagnostic env) of
  Just (TcRnSolverReport (SolverReportWithCtxt _ content) _) -> case content of
    ReportHoleError _ holeError -> Right (classifyHole holeError)
    Mismatch {mismatchMsg = mm} -> mismatch mm
    CannotUnifyVariable {mismatchMsg = mm} -> mismatch mm
    CannotResolveInstance {cannotResolve_item = item, cannotResolve_unifiers = unifiers} ->
      unresolved file bindings (errMsgSpan env) (errorItemPred item) unifiers
    _ -> Right Unexplained
  _ -> Right Unexplained
  where
    mismatch :: MismatchMsg -> Either InsightError Classified
    mismatch = maybe (Right Unexplained) (explain file bindings (errMsgSpan env)) . mismatchParts

-- | A class constraint GHC could not solve, with the instances GHC lists as
-- potentially matching it.
unresolved :: FilePath -> [Binding] -> SrcSpan -> Type -> [ClsInst] -> Either InsightError Classified
unresolved file bindings loc predicate unifiers = case loc of
  RealSrcSpan rs _ -> Unresolved <$> buildInstance file bindings rs predicate unifiers
  UnhelpfulSpan _ -> Right Unexplained

buildInstance :: FilePath -> [Binding] -> RealSrcSpan -> Type -> [ClsInst] -> Either InsightError UnresolvedInstance
buildInstance file bindings rs predicate unifiers =
  UnresolvedInstance
    <$> convertSpan file rs
    <*> convertText file (renderUser predicate)
    <*> traverse (fromDomain file . mkInstanceText . T.pack . renderUser) unifiers
    <*> contextAt file bindings rs

-- | The hints GHC attached to a diagnostic, anchored at its span. A
-- diagnostic with no hints, or with no source location, yields none.
suggestionsOf :: FilePath -> MsgEnvelope GhcMessage -> Either InsightError (Maybe Suggested)
suggestionsOf file env = case (errMsgSpan env, nonEmpty (diagnosticHints (errMsgDiagnostic env))) of
  (RealSrcSpan rs _, Just hints) -> Just <$> (Suggested <$> convertSpan file rs <*> traverse (suggestion file) hints)
  _ -> Right Nothing

suggestion :: FilePath -> GhcHint -> Either InsightError Suggestion
suggestion file hint = fromDomain file (mkSuggestion (hintKind hint) (T.pack (renderUser hint)))

-- | The kind of a hint. A kind not listed here is 'OtherHint'.
hintKind :: GhcHint -> HintKind
hintKind = \case
  SuggestSimilarNames {} -> SimilarNames
  ImportSuggestion {} -> Import
  SuggestExtension {} -> Extension
  SuggestAddTypeSignatures {} -> Signature
  SuggestFillInWildcardConstraint {} -> WildcardConstraint
  SuggestParentheses {} -> Parentheses
  SuggestMissingDo {} -> MissingDo
  SuggestLetInDo {} -> LetInDo
  SuggestBindToWildcard {} -> BindToWildcard
  _ -> OtherHint

-- | GHC reports an out-of-scope variable as a hole error too, but it is not a
-- typed hole and the plugin never sees it, so it is an unexplained error.
classifyHole :: HoleError -> Classified
classifyHole = \case
  OutOfScopeHole _ _ -> Unexplained
  HoleError {} -> HoleDiagnostic

typecheckerMessage :: GhcMessage -> Maybe TcRnMessage
typecheckerMessage = \case
  GhcTcRnMessage m -> Just (unwrap m)
  _ -> Nothing

-- | Strip the context wrappers GHC puts around a diagnostic.
--
-- Structural recursion that approximates 'until' on a fixed point; no
-- combinator fits because the step pattern-matches two different wrappers.
-- It terminates because every step strips one constructor.
unwrap :: TcRnMessage -> TcRnMessage
unwrap = \case
  TcRnMessageWithInfo _ (TcRnMessageDetailed _ inner) -> unwrap inner
  TcRnWithHsDocContext _ inner -> unwrap inner
  other -> other

-- | Mismatches that say which side was expected are the only ones explained.
mismatchParts :: MismatchMsg -> Maybe MismatchParts
mismatchParts = \case
  TypeEqMismatch
    { teq_mismatch_ty1 = l,
      teq_mismatch_ty2 = r,
      teq_mismatch_expected = e,
      teq_mismatch_actual = a
    } -> Just (MismatchParts e a l r)
  BasicMismatch {mismatch_ea = EA _, mismatch_ty1 = e, mismatch_ty2 = a} ->
    Just (MismatchParts e a e a)
  _ -> Nothing

explain :: FilePath -> [Binding] -> SrcSpan -> MismatchParts -> Either InsightError Classified
explain file bindings loc parts = case loc of
  RealSrcSpan rs _ -> Explained <$> build file bindings rs parts
  UnhelpfulSpan _ -> Right Unexplained

build :: FilePath -> [Binding] -> RealSrcSpan -> MismatchParts -> Either InsightError Explanation
build file bindings rs parts =
  make
    <$> convertSpan file rs
    <*> text (partsExpected parts)
    <*> text (partsActual parts)
    <*> text (partsLeft parts)
    <*> text (partsRight parts)
    <*> contextAt file bindings rs
  where
    text :: Type -> Either InsightError TypeText
    text = convertText file . renderUser
    make :: Span -> TypeText -> TypeText -> TypeText -> TypeText -> Context -> Explanation
    make sp e a l r ctx =
      Explanation
        { explanationSpan = sp,
          explanationExpected = e,
          explanationActual = a,
          explanationDivergence = divergence e a l r,
          explanationContext = ctx
        }

-- | The divergence is only worth showing when it says more than the expected
-- and actual types already do. The pair GHC could not match is the same pair
-- as expected and actual, in either order, in the plain case; only when GHC
-- matched deeper sub-types (a type synonym expanded, say) is there news.
divergence :: TypeText -> TypeText -> TypeText -> TypeText -> Maybe Divergence
divergence e a l r
  | (l, r) == (e, a) || (l, r) == (a, e) = Nothing
  | otherwise = either (const Nothing) Just (mkDivergence l r)
