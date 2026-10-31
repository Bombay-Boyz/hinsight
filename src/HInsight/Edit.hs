-- |
-- Module      : HInsight.Edit
-- Description : A replacement of the text of a span, and how a set of them is applied.
module HInsight.Edit
  ( Edit,
    mkEdit,
    editFile,
    editSpan,
    editText,
    EditSet,
    mkEditSet,
    editSetEdits,
    EditError (..),
    applyEditSet,
    offsetOf,
    renderEditError,
  )
where

import Data.Bits (shiftL, shiftR)
import Data.Foldable (traverse_)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import HInsight.Error (DomainError (..))
import HInsight.Source
  ( Position,
    SourceFile,
    Span,
    positionColumn,
    positionLine,
    renderPosition,
    sourceFilePath,
    spanEnd,
    spanStart,
    unColumn,
    unLine,
  )

-- | Replace the text of a span with other text; an empty span inserts.
-- Invariant: the text has no NUL character.
data Edit = UnsafeEdit
  { editFile :: !SourceFile,
    editSpan :: !Span,
    editText :: !Text
  }
  deriving stock (Eq, Ord, Show)

-- | Build an 'Edit'; the text must not hold a NUL character.
mkEdit :: SourceFile -> Span -> Text -> Either DomainError Edit
mkEdit file sp t
  | T.any (== '\0') t = Left EditTextHasNul
  | otherwise = Right (UnsafeEdit file sp t)

-- | Edits of which no two share a character of one file, in order of position.
newtype EditSet = UnsafeEditSet [Edit]
  deriving stock (Eq, Show)

-- | Build an 'EditSet'. Two overlapping edits of one file are refused, never merged.
mkEditSet :: [Edit] -> Either DomainError EditSet
mkEditSet es = UnsafeEditSet sorted <$ traverse_ disjoint (zip sorted (drop 1 sorted))
  where
    sorted :: [Edit]
    sorted = sortOn (\e -> (editFile e, spanStart (editSpan e), spanEnd (editSpan e), editText e)) es
    disjoint :: (Edit, Edit) -> Either DomainError ()
    disjoint (a, b)
      | editFile a /= editFile b = Right ()
      | spanEnd (editSpan a) <= spanStart (editSpan b) = Right ()
      | otherwise = Left (OverlappingEdits (edgeOf a) (edgeOf b))
    edgeOf :: Edit -> (Int, Int)
    edgeOf e = (unLine (positionLine (spanStart (editSpan e))), unColumn (positionColumn (spanStart (editSpan e))))

-- | The edits, in order of position.
editSetEdits :: EditSet -> [Edit]
editSetEdits (UnsafeEditSet es) = es

-- | Why an edit set could not be applied to the text it was made for.
data EditError
  = -- | The text of a file of the set was not supplied.
    NoTextFor SourceFile
  | -- | A position is not one the text has: the text changed after the edit was made.
    PositionOutsideText SourceFile Position
  deriving stock (Eq, Show)

-- | The text of every file of the set after its edits. The caller supplies
-- the text, so this runs with no files.
applyEditSet :: (SourceFile -> Maybe Text) -> EditSet -> Either EditError (Map SourceFile Text)
applyEditSet textOf (UnsafeEditSet es) = M.traverseWithKey one (M.fromListWith (flip (<>)) [(editFile e, [e]) | e <- es])
  where
    one :: SourceFile -> [Edit] -> Either EditError Text
    one file edits = do
      original <- maybe (Left (NoTextFor file)) Right (textOf file)
      located <- traverse (locate file original) edits
      pure (foldl' splice original (reverse located))
    locate :: SourceFile -> Text -> Edit -> Either EditError (Int, Int, Text)
    locate file original e = do
      start <- maybe (Left (PositionOutsideText file (spanStart (editSpan e)))) Right (offsetOf original (spanStart (editSpan e)))
      end <- maybe (Left (PositionOutsideText file (spanEnd (editSpan e)))) Right (offsetOf original (spanEnd (editSpan e)))
      pure (start, end, editText e)
    splice :: Text -> (Int, Int, Text) -> Text
    splice t (start, end, new) = T.take start t <> new <> T.drop end t

-- | The character offset of a position, or 'Nothing' when the text has no such
-- position: a line past the end, a column past the end of its line, or a column
-- inside the width of a tab. A tab advances the column to the next multiple of
-- eight plus one, as GHC counts.
offsetOf :: Text -> Position -> Maybe Int
offsetOf src pos = do
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

lineStart :: Text -> Int -> Maybe (Int, Text)
lineStart src wanted = go 1 0 (T.splitOn "\n" src)
  where
    go :: Int -> Int -> [Text] -> Maybe (Int, Text)
    go _ _ [] = Nothing
    go !n !offset (l : ls)
      | n == wanted = Just (offset, l)
      | otherwise = go (n + 1) (offset + T.length l + 1) ls

advanceColumn :: Int -> Char -> Int
advanceColumn col = \case
  '\t' -> (((col - 1) `shiftR` 3) + 1) `shiftL` 3 + 1
  _ -> col + 1

-- | Render an 'EditError'.
renderEditError :: EditError -> Text
renderEditError = \case
  NoTextFor file -> "no text was supplied for " <> T.pack (sourceFilePath file)
  PositionOutsideText file pos ->
    "the position " <> renderPosition pos <> " is not in " <> T.pack (sourceFilePath file) <> "; the file changed after the edit was made"
