-- |
-- Module      : HInsight.Report
-- Description : Plain-text reports of what an analysis found.
--
-- Pure functions from results to 'Text'. Nothing is drawn or printed here;
-- a front end decides where the text goes.
module HInsight.Report
  ( renderHoleReport,
    renderAnalysis,
    renderEnvironmentSummary,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import HInsight.Analysis (Analysis (..))
import HInsight.Environment (EnvironmentSummary, summaryComponent, summaryDropped, summaryFlagCount, unComponentName, unGhcFlag)
import HInsight.Explanation (renderExplanation)
import HInsight.Flow (renderPipeline)
import HInsight.Graph (graphImportCount, renderGraph)
import HInsight.Hole
  ( HoleFit (..),
    HoleReport (..),
    Locality (..),
  )
import HInsight.Identifier (unIdentifier)
import HInsight.Instance (renderUnresolvedInstance)
import HInsight.Source (renderPosition, spanStart)
import HInsight.Suggestion (renderSuggested)
import HInsight.TypeText (unTypeText)

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

-- | Every finding, sections separated by a blank line. The module graph is
-- shown only when some module imports another. An analysis that found nothing
-- says so rather than printing nothing.
renderAnalysis :: Analysis -> Text
renderAnalysis a = T.intercalate "\n" (findings <> environment)
  where
    findings :: [Text]
    findings
      | null sections = ["nothing to report: no type mismatches, typed holes or pipelines found\n"]
      | otherwise = sections
    sections :: [Text]
    sections =
      map renderExplanation (analysisMismatches a)
        <> map renderUnresolvedInstance (analysisInstances a)
        <> map renderSuggested (analysisSuggestions a)
        <> map renderHoleReport (analysisHoles a)
        <> map renderPipeline (analysisFlows a)
        <> [renderGraph (analysisGraph a) | graphImportCount (analysisGraph a) > 0]
        <> unexplainedLine (analysisUnexplained a)
    environment :: [Text]
    environment = maybe [] (pure . renderEnvironmentSummary) (analysisEnvironment a)

-- | One line naming the environment an analysis ran in, its flag count, and
-- the flags that were left out of the session, ending in a newline.
renderEnvironmentSummary :: EnvironmentSummary -> Text
renderEnvironmentSummary s =
  "environment: component "
    <> unComponentName (summaryComponent s)
    <> ", "
    <> T.pack (show n)
    <> (if n == 1 then " flag" else " flags")
    <> leftOut
    <> "\n"
  where
    n :: Int
    n = summaryFlagCount s
    leftOut :: Text
    leftOut = case summaryDropped s of
      [] -> ""
      dropped -> "; left out of the session: " <> T.intercalate ", " (map unGhcFlag dropped)

unexplainedLine :: Int -> [Text]
unexplainedLine n
  | n <= 0 = []
  | otherwise = [T.pack (show n) <> " other " <> noun <> " not explained by this version\n"]
  where
    noun :: Text
    noun = if n == 1 then "error" else "errors"
