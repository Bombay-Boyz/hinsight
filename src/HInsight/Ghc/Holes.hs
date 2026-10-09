-- |
-- Module      : HInsight.Ghc.Holes
-- Description : Turn captured hole data into ranked hole reports.
module HInsight.Ghc.Holes
  ( holeReports,
  )
where

import Data.Text qualified as T
import HInsight.Error (InsightError)
import HInsight.Ghc.Convert (convertSpan, convertText, fromDomain)
import HInsight.Ghc.HoleCapture (CapturedFit (..), CapturedHole (..))
import HInsight.Hole
  ( HoleFit (..),
    HoleReport (..),
    Locality (..),
    RefinementLevel,
    mkRefinementLevel,
    rankFits,
  )
import HInsight.Identifier (Identifier, mkIdentifier)

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
identifier file = fromDomain file . mkIdentifier . T.pack

refinement :: FilePath -> Int -> Either InsightError RefinementLevel
refinement file = fromDomain file . mkRefinementLevel
