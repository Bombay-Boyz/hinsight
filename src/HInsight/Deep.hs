-- |
-- Module      : HInsight.Deep
-- Description : The questions that cost a second pass of GHC over the file.
--
-- 'HInsight.Analysis.Insight' answers from one typecheck. The questions here
-- each run GHC again with more of its own output switched on, which costs
-- several times a typecheck (measured at about 4.5 times for the trace), so
-- they are asked for, never part of an ordinary analysis. This is a separate
-- record so that 'HInsight.Analysis.Insight' keeps its meaning and its cost.
--
-- Extension axis: a field is added in the change that makes it reachable.
module HInsight.Deep
  ( Deep (..),
  )
where

import HInsight.Chain (ChainView, Chains)
import HInsight.Error (InsightError)
import HInsight.Source (SourceFile)

-- | The capabilities that need a second pass, over an effect @m@.
newtype Deep m = Deep
  { -- | What GHC logged on the way to each of the file's errors, in this view.
    chainsFor :: ChainView -> SourceFile -> m (Either InsightError Chains)
  }
