-- |
-- Module      : HInsight.Tadka.Locate
-- Description : From GHC's line and column to the character offsets Tadka wants.
--
-- Tadka locates a span by the offset of its first character and its length in
-- characters. The conversion from a line and column, with GHC's tab rule, is
-- 'HInsight.Edit.offsetOf'.
module HInsight.Tadka.Locate
  ( offsetsOf,
  )
where

import Data.Text (Text)
import HInsight.Edit (offsetOf)
import HInsight.Source (Span, spanEnd, spanStart)

-- | The offset and the length, in characters, of a span; 'Nothing' when either
-- end is not a position of the text.
offsetsOf :: Text -> Span -> Maybe (Int, Int)
offsetsOf src sp = do
  start <- offsetOf src (spanStart sp)
  end <- offsetOf src (spanEnd sp)
  pure (start, end - start)
