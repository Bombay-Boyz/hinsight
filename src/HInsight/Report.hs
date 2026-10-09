-- |
-- Module      : HInsight.Report
-- Description : Plain-text reports of what an analysis found.
--
-- Pure functions from results to 'Text'. Nothing is drawn or printed here;
-- a front end decides where the text goes.
module HInsight.Report
  ( renderHoleReport,
    renderAnalysis,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import HInsight.Analysis (Analysis (..))
import HInsight.Explanation (renderExplanation, unTypeText)
import HInsight.Hole
  ( HoleFit (..),
    HoleReport (..),
    Locality (..),
    unIdentifier,
  )
import HInsight.Source (renderPosition, spanStart)

-- | One typed hole: a header line, then either "no fits found" or a
-- "fits, best first" line followed by one line per fit, in the order given.
renderHoleReport :: HoleReport -> Text
renderHoleReport h = T.unlines (header : body)
  where
    header :: Text
    header =
      "typed hole "
        <> unIdentifier (holeName h)
        <> " at "
        <> renderPosition (spanStart (holeSpan h))
        <> ", wanted type: "
        <> unTypeText (holeType h)
    body :: [Text]
    body = case holeFits h of
      [] -> ["  no fits found"]
      fits -> "  fits, best first:" : map renderFit fits

renderFit :: HoleFit -> Text
renderFit f =
  "    "
    <> unIdentifier (fitName f)
    <> " :: "
    <> unTypeText (fitType f)
    <> "  ["
    <> locality (fitLocality f)
    <> newHoles (fitMatches f)
    <> "]"
  where
    locality :: Locality -> Text
    locality = \case
      Local -> "local"
      Imported -> "imported"
    newHoles :: [a] -> Text
    newHoles [] = ""
    newHoles _ = ", leaves new holes: " <> T.intercalate ", " (map unTypeText (fitMatches f))

-- | Every finding, sections separated by a blank line. An analysis that found
-- nothing says so rather than printing nothing.
renderAnalysis :: Analysis -> Text
renderAnalysis a
  | null sections = "nothing to report: no type mismatches or typed holes found\n"
  | otherwise = T.intercalate "\n" sections
  where
    sections :: [Text]
    sections =
      map renderExplanation (analysisMismatches a)
        <> map renderHoleReport (analysisHoles a)
        <> unexplainedLine (analysisUnexplained a)

unexplainedLine :: Int -> [Text]
unexplainedLine n
  | n <= 0 = []
  | otherwise = [T.pack (show n) <> " other " <> noun <> " not explained by this version\n"]
  where
    noun :: Text
    noun = if n == 1 then "error" else "errors"
