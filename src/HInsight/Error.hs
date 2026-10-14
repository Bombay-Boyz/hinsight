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
    EnvironmentError (..),
    renderError,
    renderDomainError,
    renderEnvironmentError,
  )
where

import Data.Text (Text)
import Data.Text qualified as T

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
    -- 'HInsight.TypeText.mkTypeText' and 'HInsight.Identifier.mkIdentifier' and
    -- 'HInsight.Flow.mkStageText' and 'HInsight.Graph.mkModuleName'; the field names
    -- which input it was.
    BlankText Text
  | -- | A divergence between two identical types was requested. Raised by
    -- 'HInsight.Explanation.mkDivergence'; carries the shared text.
    IdenticalDivergence Text
  | -- | A refinement level was negative. Raised by
    -- 'HInsight.Hole.mkRefinementLevel'.
    NegativeRefinement Int
  | -- | A pipeline was built with fewer than two stages. Raised by
    -- 'HInsight.Flow.mkPipeline'; carries the number given.
    TooFewStages Int
  | -- | A module was listed twice in a project graph. Raised by
    -- 'HInsight.Graph.mkProjectGraph'; carries the module name.
    DuplicateModule Text
  | -- | A module imports a module that is not in the project graph. Raised by
    -- 'HInsight.Graph.mkProjectGraph'; carries the importing module and the
    -- missing one.
    UnknownImport Text Text
  | -- | A project root was not an absolute path. Raised by
    -- 'HInsight.Environment.mkProjectRoot'; carries the path given.
    RelativeRoot FilePath
  | -- | A compiler flag was empty or held a NUL character. Raised by
    -- 'HInsight.Environment.mkGhcFlag'; carries the flag as given.
    InvalidFlag Text
  | -- | A compiler version was not dotted decimal numbers. Raised by
    -- 'HInsight.Environment.mkGhcVersion' and 'parseGhcVersion'; carries the
    -- text given.
    MalformedVersion Text
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
  | -- | The file was not among the modules GHC found when it chased the
    -- file's imports. The field is the file. This should not happen for a file
    -- GHC accepted as a target.
    TargetNotInGraph FilePath
  | -- | A module the file imports from its own project did not compile, so the
    -- file was not analysed. The first field is the file; the second is what
    -- GHC reported, on one line.
    DependencyFailed FilePath Text
  deriving stock (Eq, Show)

-- | GHC's data could not be turned into a domain value.
data ExtractionError
  = -- | A value reported by GHC (a span, a type, an identifier or a level)
    -- violated its construction invariant. The path is the file it was
    -- reported in.
    InvalidFromGhc FilePath DomainError
  deriving stock (Eq, Show)

-- | A project environment could not be used. Raised by
-- "HInsight.Environment". It joins 'InsightError' in the change that runs an
-- analysis in an environment, which is what makes it reachable from there
-- (Standard 1.5).
data EnvironmentError
  = -- | The environment was captured for another compiler than the one this
    -- package supports. Interface files and package databases of another GHC
    -- cannot be read. The first field is the version found, the second the
    -- version supported.
    CompilerMismatch Text Text
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
  TooFewStages n -> "a pipeline needs at least 2 stages but was given " <> tshow n
  DuplicateModule m -> "module " <> m <> " is listed twice in the project graph"
  UnknownImport m missing -> "module " <> m <> " imports " <> missing <> ", which is not in the project graph"
  RelativeRoot p -> "the project root " <> T.pack p <> " is not an absolute path"
  InvalidFlag f -> "the compiler flag " <> T.pack (show f) <> " is empty or contains a NUL character"
  MalformedVersion v -> "the compiler version " <> T.pack (show v) <> " is not dotted decimal numbers such as 9.10.3"
  where
    pos :: Int -> Int -> Text
    pos l c = tshow l <> ":" <> tshow c

renderSession :: SessionError -> Text
renderSession = \case
  LibDirNotFound p -> "the GHC library directory does not exist: " <> T.pack p
  SourceFileNotFound p -> "the file to analyse does not exist: " <> T.pack p
  GhcThrew p msg -> "GHC threw while analysing " <> T.pack p <> ": " <> msg
  TargetNotInGraph p -> "GHC did not list " <> T.pack p <> " among the modules it found for it"
  DependencyFailed p why ->
    "a module imported by " <> T.pack p <> " did not compile, so it was not analysed: " <> why

renderExtraction :: ExtractionError -> Text
renderExtraction = \case
  InvalidFromGhc p e -> "GHC reported an unusable value in " <> T.pack p <> ": " <> renderDomainError e

tshow :: Int -> Text
tshow = T.pack . show

-- | Render an 'EnvironmentError'.
renderEnvironmentError :: EnvironmentError -> Text
renderEnvironmentError = \case
  CompilerMismatch found expected ->
    "the project environment was captured for GHC "
      <> found
      <> " but this package supports only GHC "
      <> expected
      <> "; rebuild the project with ghc-"
      <> expected
