-- |
-- Module      : HInsight.Ghc.Convert
-- Description : Turn GHC's span and type values into domain values.
--
-- Every conversion that can fail returns 'InsightError' tagged with the file
-- GHC was reporting on, so a failure names where it happened.
module HInsight.Ghc.Convert
  ( convertSpan,
    convertText,
    renderGhcType,
    renderGhc,
  )
where

import Data.Bifunctor (first)
import qualified Data.Text as T
import GHC.Core.Type (Type)
import GHC.Types.SrcLoc (RealSrcSpan, srcSpanEndCol, srcSpanEndLine, srcSpanStartCol, srcSpanStartLine)
import GHC.Utils.Outputable (SDoc, defaultSDocContext, ppr, showSDocOneLine)
import HInsight.Error (DomainError, ExtractionError (..), InsightError (..))
import HInsight.Explanation (TypeText, mkTypeText)
import HInsight.Source (Position, Span, mkColumn, mkLine, mkPosition, mkSpan)

-- | Convert a GHC span. Lines and columns are 1-based and the end column is
-- exclusive, which matches "HInsight.Source".
convertSpan :: FilePath -> RealSrcSpan -> Either InsightError Span
convertSpan file rs = first (invalid file) (start >>= \s -> end >>= mkSpan s)
  where
    start :: Either DomainError Position
    start = position (srcSpanStartLine rs) (srcSpanStartCol rs)
    end :: Either DomainError Position
    end = position (srcSpanEndLine rs) (srcSpanEndCol rs)

position :: Int -> Int -> Either DomainError Position
position l c = mkPosition <$> mkLine l <*> mkColumn c

-- | Convert text GHC rendered into a 'TypeText'.
convertText :: FilePath -> String -> Either InsightError TypeText
convertText file = first (invalid file) . mkTypeText . T.pack

-- | Render a GHC type on one line with default settings.
renderGhcType :: Type -> String
renderGhcType = renderGhc . ppr

-- | Render any GHC document on one line with default settings.
renderGhc :: SDoc -> String
renderGhc = showSDocOneLine defaultSDocContext

invalid :: FilePath -> DomainError -> InsightError
invalid file = ExtractionFailure . InvalidFromGhc file
