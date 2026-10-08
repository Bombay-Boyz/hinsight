-- |
-- Module      : HInsight.Error
-- Description : The closed vocabulary of every way this package can fail.
--
-- This is the single place to see the whole universe of failures (Standard
-- 2.15). The top-level type is a closed sum of closed sums, one case per
-- pipeline stage (Standard 2.3): building domain values, running the GHC
-- session, and converting GHC's data into domain values.
--
-- A constructor is added only in the same change that makes it reachable
-- (Standard 1.5). 'DomainError' deliberately carries primitives rather than
-- domain types so that the domain modules can depend on this one without a
-- cycle.
module HInsight.Error
  ( InsightError (..),
    DomainError (..),
    SessionError (..),
    ExtractionError (..),
    renderError,
    renderDomainError,
  )
where

import Data.Text (Text)
import qualified Data.Text as T

-- | Any failure of the package, tagged by the stage that produced it.
data InsightError
  = -- | A value failed its construction invariant.
    DomainFailure DomainError
  | -- | The GHC session could not be run or did not finish.
    SessionFailure SessionError
  | -- | GHC produced data that does not map onto a domain value.
    ExtractionFailure ExtractionError
  deriving stock (Eq, Show)

-- | A smart constructor rejected its input. Raised by the pure domain layer.
data DomainError
  = -- | A file path was empty. Raised by 'HInsight.Source.mkSourceFile' and
    -- 'HInsight.Config.mkLibDir'; the field names which input it was.
    EmptyPath Text
  | -- | A line number was below 1. Raised by 'HInsight.Source.mkLine'.
    NonPositiveLine Int
  | -- | A column number was below 1. Raised by 'HInsight.Source.mkColumn'.
    NonPositiveColumn Int
  | -- | A span ended before it started: start line, start column, end line,
    -- end column. Raised by 'HInsight.Source.mkSpan'.
    SpanEndsBeforeStart Int Int Int Int
  | -- | A rendered type or identifier was empty or only whitespace. Raised by
    -- 'HInsight.Explanation.mkTypeText' and 'HInsight.Hole.mkIdentifier'; the
    -- field names which input it was.
    BlankText Text
  | -- | A divergence between two identical types was requested. Raised by
    -- 'HInsight.Explanation.mkDivergence'; carries the shared text.
    IdenticalDivergence Text
  | -- | A refinement level was negative. Raised by
    -- 'HInsight.Hole.mkRefinementLevel'.
    NegativeRefinement Int
  deriving stock (Eq, Show)

-- | The GHC session failed. Raised by "HInsight.Ghc.Session".
data SessionError
  = -- | The GHC library directory does not exist.
    LibDirNotFound FilePath
  | -- | The file to analyse does not exist.
    SourceFileNotFound FilePath
  | -- | GHC (or the file system underneath it) threw while analysing a file.
    -- The first field is the file, the second the exception rendered by its
    -- own 'Show' instance.
    GhcThrew FilePath Text
  | -- | The file's dependency analysis produced a number of modules other than
    -- one. This version analyses a single module that imports only installed
    -- packages. The fields are the file and the number of modules found.
    UnexpectedModuleCount FilePath Int
  deriving stock (Eq, Show)

-- | GHC's data could not be turned into a domain value.
data ExtractionError
  = -- | A value reported by GHC (a span, a type, an identifier or a level)
    -- violated its construction invariant. The path is the file it was
    -- reported in.
    InvalidFromGhc FilePath DomainError
  deriving stock (Eq, Show)

-- | A message that names the specific thing that failed and where.
renderError :: InsightError -> Text
renderError = \case
  DomainFailure e -> renderDomainError e
  SessionFailure e -> renderSession e
  ExtractionFailure e -> renderExtraction e

-- | Render a 'DomainError'.
renderDomainError :: DomainError -> Text
renderDomainError = \case
  EmptyPath what -> "the " <> what <> " path is empty"
  NonPositiveLine n -> "line number " <> tshow n <> " is below 1; lines start at 1"
  NonPositiveColumn n -> "column number " <> tshow n <> " is below 1; columns start at 1"
  SpanEndsBeforeStart l1 c1 l2 c2 ->
    "span ends before it starts: start "
      <> pos l1 c1
      <> ", end "
      <> pos l2 c2
  BlankText what -> "the " <> what <> " is empty or only whitespace"
  IdenticalDivergence t -> "cannot diverge: both types are identical (" <> t <> ")"
  NegativeRefinement n -> "refinement level " <> tshow n <> " is negative"
  where
    pos :: Int -> Int -> Text
    pos l c = tshow l <> ":" <> tshow c

renderSession :: SessionError -> Text
renderSession = \case
  LibDirNotFound p -> "the GHC library directory does not exist: " <> T.pack p
  SourceFileNotFound p -> "the file to analyse does not exist: " <> T.pack p
  GhcThrew p msg -> "GHC threw while analysing " <> T.pack p <> ": " <> msg
  UnexpectedModuleCount p n ->
    "expected exactly one module for "
      <> T.pack p
      <> " but found "
      <> tshow n
      <> "; this version analyses a single module that imports only installed packages"

renderExtraction :: ExtractionError -> Text
renderExtraction = \case
  InvalidFromGhc p e -> "GHC reported an unusable value in " <> T.pack p <> ": " <> renderDomainError e

tshow :: Int -> Text
tshow = T.pack . show
