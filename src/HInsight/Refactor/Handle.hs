-- |
-- Module      : HInsight.Refactor.Handle
-- Description : The refactorings, as capabilities over an effect.
--
-- A separate record from 'HInsight.Analysis.Insight' and 'HInsight.Deep.Deep',
-- because it proposes changes to source, which is a different risk class. No
-- operation writes a file: each returns a 'VerifiedPlan' that the caller
-- applies or not.
--
-- Extension axis: a field is added in the change that makes it reachable.
module HInsight.Refactor.Handle
  ( Refactor (..),
  )
where

import HInsight.Error (InsightError)
import HInsight.Refactor (FunctionName)
import HInsight.Refactor.Verify (VerifiedPlan)
import HInsight.Source (SourceFile, Span)

-- | The refactorings, over an effect @m@.
newtype Refactor m = Refactor
  { -- | The plan that moves the selected expression into a new top-level
    -- function of the given name, as GHC accepted it.
    extractFunction :: SourceFile -> Span -> FunctionName -> m (Either InsightError VerifiedPlan)
  }
