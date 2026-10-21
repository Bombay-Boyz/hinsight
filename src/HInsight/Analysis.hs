-- |
-- Module      : HInsight.Analysis
-- Description : What one analysis of a file returns, and the handle that produces it.
--
-- The 'Insight' record is the dependency-inversion boundary (Standard 0.5):
-- code that wants analyses depends on this shape, not on GHC. Tests supply a
-- pure implementation; production supplies the GHC-backed one from
-- "HInsight.Ghc.Session".
--
-- Extension axis: closed by design. The kinds of finding are fields of
-- 'Analysis', so adding one is a compile error at every site that builds one.
module HInsight.Analysis
  ( Analysis (..),
    emptyAnalysis,
    Insight (..),
  )
where

import HInsight.Environment (EnvironmentSummary)
import HInsight.Error (InsightError)
import HInsight.Explanation (Explanation)
import HInsight.Flow (Pipeline)
import HInsight.Graph (ProjectGraph, emptyGraph)
import HInsight.Hole (HoleReport)
import HInsight.Instance (UnresolvedInstance)
import HInsight.Source (SourceFile)
import HInsight.Suggestion (Suggested)

-- | Everything found in one file.
data Analysis = Analysis
  { -- | Type mismatches GHC reported, each explained.
    analysisMismatches :: ![Explanation],
    -- | Class constraints GHC could not solve, each with the instances GHC
    -- lists for it.
    analysisInstances :: ![UnresolvedInstance],
    -- | The hints GHC attached to its errors. They come in addition to the
    -- findings above and do not change how errors are counted.
    analysisSuggestions :: ![Suggested],
    -- | Typed holes with their ranked fits.
    analysisHoles :: ![HoleReport],
    -- | Pipelines of @.@, @$@ and @>>=@ with the type at each stage. The types
    -- come from the typechecked program; for a file with errors they come
    -- from a second typecheck with the errors deferred, and some errors
    -- cannot be deferred, in which case there are no pipelines.
    analysisFlows :: ![Pipeline],
    -- | The project modules reachable from the file and the imports between
    -- them. Empty when the file's imports could not be resolved.
    analysisGraph :: !ProjectGraph,
    -- | How many other diagnostics GHC reported that this version does not
    -- explain (for example scope errors). Never negative.
    analysisUnexplained :: !Int,
    -- | The project environment the file was analysed in; 'Nothing' when its
    -- place was worked out from its header instead.
    analysisEnvironment :: !(Maybe EnvironmentSummary)
  }
  deriving stock (Eq, Show)

-- | An analysis that found nothing.
emptyAnalysis :: Analysis
emptyAnalysis = Analysis [] [] [] [] [] emptyGraph 0 Nothing

-- | The capability to analyse a file, over an effect @m@.
newtype Insight m = Insight
  { analyseFile :: SourceFile -> m (Either InsightError Analysis)
  }
