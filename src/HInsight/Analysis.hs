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

import HInsight.Error (InsightError)
import HInsight.Explanation (Explanation)
import HInsight.Flow (Pipeline)
import HInsight.Hole (HoleReport)
import HInsight.Source (SourceFile)

-- | Everything found in one file.
data Analysis = Analysis
  { -- | Type mismatches GHC reported, each explained.
    analysisMismatches :: ![Explanation],
    -- | Typed holes with their ranked fits.
    analysisHoles :: ![HoleReport],
    -- | Pipelines of @.@, @$@ and @>>=@ with the type at each stage. Only
    -- found when the file typechecks, since the types come from the
    -- typechecked program.
    analysisFlows :: ![Pipeline],
    -- | How many other diagnostics GHC reported that this version does not
    -- explain (for example scope errors). Never negative.
    analysisUnexplained :: !Int
  }
  deriving stock (Eq, Show)

-- | An analysis that found nothing.
emptyAnalysis :: Analysis
emptyAnalysis = Analysis [] [] [] 0

-- | The capability to analyse a file, over an effect @m@.
newtype Insight m = Insight
  { analyseFile :: SourceFile -> m (Either InsightError Analysis)
  }
