-- |
-- Module      : HInsight.Ghc.Messages
-- Description : Explain GHC's type-mismatch diagnostics.
--
-- Works on the structured diagnostics GHC 9.10 puts in a 'SourceError', not on
-- rendered text. Only mismatches that carry GHC's expected/actual information
-- are explained; every other error is counted, never dropped silently. A real
-- typed hole is reported through the hole-fit plugin instead, and is tallied
-- here so the caller can check that the plugin saw it.
module HInsight.Ghc.Messages
  ( Extracted (..),
    extractMessages,
  )
where

import GHC.Core.Type (Type)
import GHC.Driver.Errors.Types (GhcMessage (..))
import GHC.Tc.Errors.Types
  ( ErrorItem,
    HoleError (..),
    MismatchEA (..),
    MismatchMsg (..),
    SolverReportWithCtxt (..),
    TcRnMessage (..),
    TcRnMessageDetailed (..),
    TcSolverReportMsg (..),
    errorItemOrigin,
  )
import GHC.Tc.Types.Origin (pprCtOrigin)
import GHC.Types.Error (MsgEnvelope (..))
import GHC.Types.SrcLoc (RealSrcSpan, SrcSpan (..))
import qualified Data.Text as T
import HInsight.Error (InsightError)
import HInsight.Explanation
  ( Divergence,
    Explanation (..),
    Origin (..),
    TypeText,
    mkDivergence,
  )
import HInsight.Ghc.Convert (convertSpan, convertText, renderGhc, renderGhcType)
import HInsight.Source (Span)

-- | The explained mismatches, the number of errors left unexplained, and the
-- number of typed-hole errors GHC reported (which the hole-fit plugin is
-- expected to have seen).
data Extracted = Extracted
  { extractedMismatches :: ![Explanation],
    extractedUnexplained :: !Int,
    extractedHoleDiagnostics :: !Int
  }
  deriving stock (Eq, Show)

-- | What became of one diagnostic.
data Classified
  = Explained Explanation
  | -- | A typed hole or wildcard: its fits come through the hole-fit plugin.
    HoleDiagnostic
  | Unexplained

-- | The pieces of a mismatch GHC reported with an expected and an actual type.
data MismatchParts = MismatchParts
  { partsItem :: ErrorItem,
    partsExpected :: Type,
    partsActual :: Type,
    -- | The two sub-types at which the mismatch was detected.
    partsLeft :: Type,
    partsRight :: Type
  }

-- | Classify and explain a list of error diagnostics from one file.
extractMessages :: FilePath -> [MsgEnvelope GhcMessage] -> Either InsightError Extracted
extractMessages file envs = tally <$> traverse (classify file) envs

-- | Count by outcome. List comprehensions with a refutable pattern are the
-- filter-and-map here; 'length' is strict, so nothing accumulates lazily.
tally :: [Classified] -> Extracted
tally cs =
  Extracted
    { extractedMismatches = [e | Explained e <- cs],
      extractedUnexplained = length [() | Unexplained <- cs],
      extractedHoleDiagnostics = length [() | HoleDiagnostic <- cs]
    }

classify :: FilePath -> MsgEnvelope GhcMessage -> Either InsightError Classified
classify file env = case typecheckerMessage (errMsgDiagnostic env) of
  Just (TcRnSolverReport (SolverReportWithCtxt _ content) _) -> case content of
    ReportHoleError _ holeError -> Right (classifyHole holeError)
    Mismatch {mismatchMsg = mm} -> maybe (Right Unexplained) (explain file (errMsgSpan env)) (mismatchParts mm)
    _ -> Right Unexplained
  _ -> Right Unexplained

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
    { teq_mismatch_item = item,
      teq_mismatch_ty1 = l,
      teq_mismatch_ty2 = r,
      teq_mismatch_expected = e,
      teq_mismatch_actual = a
    } -> Just (MismatchParts item e a l r)
  BasicMismatch {mismatch_ea = EA _, mismatch_item = item, mismatch_ty1 = e, mismatch_ty2 = a} ->
    Just (MismatchParts item e a e a)
  _ -> Nothing

explain :: FilePath -> SrcSpan -> MismatchParts -> Either InsightError Classified
explain file loc parts = case loc of
  RealSrcSpan rs _ -> Explained <$> build file rs parts
  UnhelpfulSpan _ -> Right Unexplained

build :: FilePath -> RealSrcSpan -> MismatchParts -> Either InsightError Explanation
build file rs parts =
  make
    <$> convertSpan file rs
    <*> text (partsExpected parts)
    <*> text (partsActual parts)
    <*> text (partsLeft parts)
    <*> text (partsRight parts)
  where
    text :: Type -> Either InsightError TypeText
    text = convertText file . renderGhcType
    make :: Span -> TypeText -> TypeText -> TypeText -> TypeText -> Explanation
    make sp e a l r =
      Explanation
        { explanationSpan = sp,
          explanationExpected = e,
          explanationActual = a,
          explanationDivergence = divergence e a l r,
          explanationOrigin = origin (partsItem parts)
        }

-- | The divergence is only worth showing when the detected sub-types differ
-- from each other and from the overall pair. Two sub-types that render
-- identically (they can differ only in invisible detail) give no divergence.
divergence :: TypeText -> TypeText -> TypeText -> TypeText -> Maybe Divergence
divergence e a l r
  | (l, r) == (e, a) = Nothing
  | otherwise = either (const Nothing) Just (mkDivergence l r)

origin :: ErrorItem -> Origin
origin = Origin . T.pack . renderGhc . pprCtOrigin . errorItemOrigin
