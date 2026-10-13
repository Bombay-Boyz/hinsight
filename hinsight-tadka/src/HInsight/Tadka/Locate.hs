-- |
-- Module      : HInsight.Tadka.Locate
-- Description : From GHC's line and column to the character offsets Tadka wants.
--
-- GHC reports a position as a line and a column. A column counts characters,
-- except that a tab advances it to the next tab stop, a multiple of eight plus
-- one (@advance_tabstop@ in GHC's @GHC.Types.SrcLoc@). Tadka locates a span by
-- the offset of its first character and its length in characters. This module
-- converts one into the other, so a tab earlier in a line cannot shift a label.
module HInsight.Tadka.Locate
  ( offsetAt,
    offsetsOf,
  )
where

import Data.Bits (shiftL, shiftR)
import Data.Text (Text)
import Data.Text qualified as T
import HInsight.Source
  ( Position,
    Span,
    positionColumn,
    positionLine,
    spanEnd,
    spanStart,
    unColumn,
    unLine,
  )

-- | The character offset of a position in the text, or 'Nothing' when the
-- position is not one the text has: a line past the end, a column past the end
-- of its line, or a column inside the width of a tab.
offsetAt :: Text -> Position -> Maybe Int
offsetAt src pos = do
  (lineOffset, line) <- lineStart src (unLine (positionLine pos))
  (lineOffset +) <$> walk line 1 0
  where
    target :: Int
    target = unColumn (positionColumn pos)
    walk :: Text -> Int -> Int -> Maybe Int
    walk rest !col !idx
      | col == target = Just idx
      | col > target = Nothing
      | otherwise = case T.uncons rest of
          Nothing -> Nothing
          Just (c, more) -> walk more (advanceColumn col c) (idx + 1)

-- | The offset and the length, in characters, of a span; 'Nothing' when either
-- end is not a position of the text.
offsetsOf :: Text -> Span -> Maybe (Int, Int)
offsetsOf src sp = do
  start <- offsetAt src (spanStart sp)
  end <- offsetAt src (spanEnd sp)
  pure (start, end - start)

-- | The offset at which a line starts, and the line's text without its
-- newline. Every line has an entry, including the empty one after a final
-- newline.
lineStart :: Text -> Int -> Maybe (Int, Text)
lineStart src wanted = go 1 0 (T.splitOn "\n" src)
  where
    go :: Int -> Int -> [Text] -> Maybe (Int, Text)
    go _ _ [] = Nothing
    go !n !offset (l : ls)
      | n == wanted = Just (offset, l)
      | otherwise = go (n + 1) (offset + T.length l + 1) ls

-- | Where the column goes after this character, as GHC counts.
advanceColumn :: Int -> Char -> Int
advanceColumn col = \case
  '\t' -> (((col - 1) `shiftR` 3) + 1) `shiftL` 3 + 1
  _ -> col + 1
