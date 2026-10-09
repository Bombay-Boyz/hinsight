-- |
-- Module      : HInsight.Source
-- Description : Where things are in a source file.
--
-- Positions follow GHC: lines and columns start at 1, and the end of a span is
-- exclusive. Every type here has an unexported constructor; the only way to
-- obtain a value is a smart constructor (Standard 1.3, 3.3).
module HInsight.Source
  ( SourceFile,
    mkSourceFile,
    sourceFilePath,
    Line,
    mkLine,
    unLine,
    Column,
    mkColumn,
    unColumn,
    Position,
    mkPosition,
    positionLine,
    positionColumn,
    Span,
    mkSpan,
    spanStart,
    spanEnd,
    renderPosition,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import HInsight.Error (DomainError (..))

-- | A path to a Haskell source file. Invariant: not empty.
newtype SourceFile = UnsafeSourceFile FilePath
  deriving stock (Eq, Ord, Show)

-- | Build a 'SourceFile'; the path must not be empty.
mkSourceFile :: FilePath -> Either DomainError SourceFile
mkSourceFile p
  | null p = Left (EmptyPath "source file")
  | otherwise = Right (UnsafeSourceFile p)

-- | The path inside a 'SourceFile'.
sourceFilePath :: SourceFile -> FilePath
sourceFilePath (UnsafeSourceFile p) = p

-- | A line number. Invariant: at least 1.
newtype Line = UnsafeLine Int
  deriving stock (Eq, Ord, Show)

-- | Build a 'Line'; the number must be at least 1.
mkLine :: Int -> Either DomainError Line
mkLine n
  | n >= 1 = Right (UnsafeLine n)
  | otherwise = Left (NonPositiveLine n)

-- | The number inside a 'Line'.
unLine :: Line -> Int
unLine (UnsafeLine n) = n

-- | A column number. Invariant: at least 1.
newtype Column = UnsafeColumn Int
  deriving stock (Eq, Ord, Show)

-- | Build a 'Column'; the number must be at least 1.
mkColumn :: Int -> Either DomainError Column
mkColumn n
  | n >= 1 = Right (UnsafeColumn n)
  | otherwise = Left (NonPositiveColumn n)

-- | The number inside a 'Column'.
unColumn :: Column -> Int
unColumn (UnsafeColumn n) = n

-- | A point in a file. The derived 'Ord' compares line first, then column,
-- which is reading order.
data Position = Position
  { positionLine :: !Line,
    positionColumn :: !Column
  }
  deriving stock (Eq, Ord, Show)

-- | Pair a line and a column.
mkPosition :: Line -> Column -> Position
mkPosition = Position

-- | A region of a file. Invariant: the start is not after the end.
data Span = UnsafeSpan !Position !Position
  deriving stock (Eq, Ord, Show)

-- | Build a 'Span'. A zero-width span (start equal to end) is allowed.
mkSpan :: Position -> Position -> Either DomainError Span
mkSpan s e
  | s <= e = Right (UnsafeSpan s e)
  | otherwise =
      Left
        ( SpanEndsBeforeStart
            (unLine (positionLine s))
            (unColumn (positionColumn s))
            (unLine (positionLine e))
            (unColumn (positionColumn e))
        )

-- | Where a 'Span' starts.
spanStart :: Span -> Position
spanStart (UnsafeSpan s _) = s

-- | Where a 'Span' ends (exclusive).
spanEnd :: Span -> Position
spanEnd (UnsafeSpan _ e) = e

-- | A position as @line:column@, the form editors and compilers use.
renderPosition :: Position -> Text
renderPosition p = T.pack (show (unLine (positionLine p))) <> ":" <> T.pack (show (unColumn (positionColumn p)))
