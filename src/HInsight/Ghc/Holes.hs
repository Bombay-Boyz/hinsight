-- |
-- Module      : HInsight.Ghc.Holes
-- Description : Turn captured hole data into ranked hole reports.
module HInsight.Ghc.Holes
  ( holeReports,
  )
where

import Data.Bifunctor (first)
import Data.Text qualified as T
import HInsight.Error (DomainError, ExtractionError (..), InsightError (..))
import HInsight.Ghc.Convert (convertSpan, convertText)
import HInsight.Ghc.HoleCapture (CapturedFit (..), CapturedHole (..))
import HInsight.Hole
  ( HoleFit (..),
    HoleReport (..),
    Identifier,
    Locality (..),
    RefinementLevel,
    mkIdentifier,
    mkRefinementLevel,
    rankFits,
  )

-- | Convert every captured hole; the first invalid value stops the conversion
-- and names the file.
holeReports :: FilePath -> [CapturedHole] -> Either InsightError [HoleReport]
holeReports file = traverse (holeReport file)

holeReport :: FilePath -> CapturedHole -> Either InsightError HoleReport
holeReport file h =
  HoleReport
    <$> identifier file (capturedHoleName h)
    <*> convertSpan file (capturedHoleSpan h)
    <*> convertText file (capturedHoleType h)
    <*> (rankFits <$> traverse (fitOf file) (capturedHoleFits h))

fitOf :: FilePath -> CapturedFit -> Either InsightError HoleFit
fitOf file f =
  HoleFit
    <$> identifier file (capturedFitName f)
    <*> convertText file (capturedFitType f)
    <*> refinement file (capturedFitRefinement f)
    <*> pure (locality (capturedFitLocal f))
    <*> traverse (convertText file) (capturedFitMatches f)

locality :: Bool -> Locality
locality isLocal = if isLocal then Local else Imported

identifier :: FilePath -> String -> Either InsightError Identifier
identifier file = invalid file . mkIdentifier . T.pack

refinement :: FilePath -> Int -> Either InsightError RefinementLevel
refinement file = invalid file . mkRefinementLevel

invalid :: FilePath -> Either DomainError a -> Either InsightError a
invalid file = first (ExtractionFailure . InvalidFromGhc file)
