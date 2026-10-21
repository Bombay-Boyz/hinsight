-- |
-- Module      : HInsight.Tadka
-- Description : Show what hinsight found as Tadka diagnostics.
--
-- hinsight returns plain values and draws nothing. This module turns an
-- 'Analysis' into 'Finding's, each an instance of Tadka's 'Td.Diagnostic', so
-- Tadka can draw them as annotated source, as sentences for a screen reader or
-- as JSON. It is a separate package so that hinsight itself depends on no
-- renderer.
module HInsight.Tadka
  ( Finding,
    FindingKind (..),
    findingKind,
    findings,
    renderExtras,
    AdaptError (..),
    renderAdaptError,
    namedSource,
  )
where

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as T
import HInsight.Analysis (Analysis (..))
import HInsight.Context (Context (..))
import HInsight.Explanation (Divergence, Explanation (..), divergenceLeft, divergenceRight)
import HInsight.Flow (FlowKind (..), Pipeline, Stage (..), pipelineContext, pipelineKind, pipelineSpan, pipelineStages, pipelineType, unStageText)
import HInsight.Graph (graphImportCount, renderGraph)
import HInsight.Hole (HoleFit (..), HoleReport (..), Locality (..))
import HInsight.Identifier (unIdentifier)
import HInsight.Instance (InstanceText, UnresolvedInstance (..), shownCandidates, unInstanceText)
import HInsight.Report (renderEnvironmentSummary)
import HInsight.Source (Span, positionColumn, positionLine, spanEnd, spanStart, unColumn, unLine)
import HInsight.Suggestion (Suggested (..), Suggestion, suggestionText)
import HInsight.Tadka.Locate (offsetsOf)
import HInsight.TypeText (TypeText, unTypeText)
import Prettyprinter (Doc, annotate, fill, pretty, vsep, (<+>))
import Tadka qualified as Td

-- | Why a finding could not be drawn.
data AdaptError
  = -- | A span of hinsight is not a position of the source text: start line,
    -- start column, end line, end column. This happens when the text given
    -- here is not the text GHC read.
    UnusableSpan Int Int Int Int
  | -- | The source was given an empty name.
    UnnamedSource
  deriving stock (Eq, Show)

renderAdaptError :: AdaptError -> Text
renderAdaptError = \case
  UnusableSpan l1 c1 l2 c2 ->
    "a finding at "
      <> pos l1 c1
      <> "-"
      <> pos l2 c2
      <> " does not lie in the source text; was the file changed after it was analysed?"
  UnnamedSource -> "the source needs a name to be shown"
  where
    pos :: Int -> Int -> Text
    pos l c = T.pack (show l) <> ":" <> T.pack (show c)

-- | A named source for Tadka: the file's path as its name, and its text.
namedSource :: FilePath -> Text -> Either AdaptError Td.NamedSource
namedSource path text = case Td.mkNamedSource (T.pack path) text of
  Left Td.EmptySourceName -> Left UnnamedSource
  Right src -> Right src

-- | What a finding reports. The kind decides its code and severity.
data FindingKind
  = MismatchKind
  | UnresolvedInstanceKind
  | HoleKind
  | PipelineKind
  | HintsKind
  deriving stock (Eq, Show, Enum, Bounded)

-- | Something hinsight found, ready for Tadka to draw. The constructor is not
-- exported: findings come from 'findings', which has already checked that
-- every label lies in the source.
data Finding = Finding
  { findingKind :: !FindingKind,
    findingMessage :: !(Doc Td.Ann),
    findingContext :: !Td.Context,
    findingHelp :: !(Maybe (Doc Td.Ann)),
    findingNotes :: ![Doc Td.Ann]
  }

-- | One line of detail under a finding: a fit, or a stage. Tadka's @help@ is a
-- single sentence, so a list is drawn as related notes, one per item.
newtype Note = Note (Doc Td.Ann)

instance Td.Diagnostic Note where
  message (Note doc) = doc
  severity _ = Td.SevAdvice

instance Td.Diagnostic Finding where
  message = findingMessage
  context = findingContext
  help = findingHelp
  related = map (Td.SomeDiagnostic . Note) . findingNotes
  code = kindCode . findingKind
  severity = kindSeverity . findingKind

-- | A code per kind. The grammar is checked at run time by Tadka, so a code
-- that fails it would be dropped; the tests assert that none is.
kindCode :: FindingKind -> Maybe Td.DiagnosticCode
kindCode kind = either (const Nothing) Just (Td.mkDiagnosticCode text)
  where
    text :: Text
    text = case kind of
      MismatchKind -> "hinsight::E0001"
      HoleKind -> "hinsight::E0002"
      PipelineKind -> "hinsight::E0003"
      UnresolvedInstanceKind -> "hinsight::E0004"
      HintsKind -> "hinsight::E0005"

-- | GHC rejects a module with a mismatch or a hole; a pipeline is only shown.
kindSeverity :: FindingKind -> Td.Severity
kindSeverity = \case
  MismatchKind -> Td.SevError
  UnresolvedInstanceKind -> Td.SevError
  HoleKind -> Td.SevError
  PipelineKind -> Td.SevAdvice
  HintsKind -> Td.SevAdvice

-- | Every finding of an analysis, mismatches first, then unresolved
-- constraints, then holes, then pipelines, then GHC's suggestions, each group
-- in the order hinsight gave it. The text must be the text GHC read. Fails on
-- the first span that is not a position of it.
findings :: Td.NamedSource -> Analysis -> Either AdaptError [Finding]
findings src a =
  concat
    <$> sequence
      [ traverse (mismatchFinding src) (analysisMismatches a),
        traverse (instanceFinding src) (analysisInstances a),
        traverse (holeFinding src) (analysisHoles a),
        traverse (pipelineFinding src) (analysisFlows a),
        concat <$> traverse (hintFindings src) (analysisSuggestions a)
      ]

-- | The parts of an analysis that are not diagnostics: the module graph, the
-- count of errors hinsight did not explain, and the environment it ran in, as
-- plain lines. Empty when
-- there is nothing to say.
renderExtras :: Analysis -> Text
renderExtras a = T.concat (graph <> unexplained <> environment)
  where
    environment :: [Text]
    environment = maybe [] (pure . renderEnvironmentSummary) (analysisEnvironment a)
    graph :: [Text]
    graph = [renderGraph (analysisGraph a) | graphImportCount (analysisGraph a) > 0]
    unexplained :: [Text]
    unexplained
      | n <= 0 = []
      | otherwise = [T.pack (show n) <> " other " <> noun <> " not explained by this version\n"]
    n :: Int
    n = analysisUnexplained a
    noun :: Text
    noun = if n == 1 then "error" else "errors"

mismatchFinding :: Td.NamedSource -> Explanation -> Either AdaptError Finding
mismatchFinding src e = do
  primary <- label src Td.Primary (explanationSpan e) ("expected" <+> typeDoc (explanationExpected e) <> ", but this has type" <+> typeDoc (explanationActual e))
  context <- contextWith src primary (explanationContext e)
  pure
    Finding
      { findingKind = MismatchKind,
        findingMessage = "type mismatch: expected" <+> typeDoc (explanationExpected e) <> ", found" <+> typeDoc (explanationActual e),
        findingContext = context,
        findingHelp = nonEmptyDocs (maybe [] divergenceNote (explanationDivergence e) <> unsignedNote (explanationContext e)),
        findingNotes = []
      }

-- | One finding per hint GHC gave for an error, at the error's span.
hintFindings :: Td.NamedSource -> Suggested -> Either AdaptError [Finding]
hintFindings src s = traverse hintFinding (toList (suggestedHints s))
  where
    hintFinding :: Suggestion -> Either AdaptError Finding
    hintFinding h = do
      primary <- label src Td.Primary (suggestedSpan s) "GHC has a suggestion for this"
      pure
        Finding
          { findingKind = HintsKind,
            findingMessage = pretty (suggestionText h),
            findingContext = Td.mkContextDegrading src (primary :| []),
            findingHelp = Nothing,
            findingNotes = []
          }

instanceFinding :: Td.NamedSource -> UnresolvedInstance -> Either AdaptError Finding
instanceFinding src u = do
  primary <- label src Td.Primary (unresolvedSpan u) ("GHC could not solve" <+> typeDoc (unresolvedConstraint u))
  context <- contextWith src primary (unresolvedContext u)
  case shownCandidates u of
    (shown, omitted) ->
      pure
        Finding
          { findingKind = UnresolvedInstanceKind,
            findingMessage = "unresolved constraint" <+> typeDoc (unresolvedConstraint u),
            findingContext = context,
            findingHelp = Just (candidatesHeading (unresolvedPotential u)),
            findingNotes = map (textDoc . unInstanceText) shown <> [pretty ("and " <> T.pack (show omitted) <> " more") | omitted > 0]
          }

candidatesHeading :: [InstanceText] -> Doc Td.Ann
candidatesHeading = \case
  [] -> "GHC lists no potentially matching instances"
  instances -> "potentially matching instances (" <> pretty (length instances) <> ")"

holeFinding :: Td.NamedSource -> HoleReport -> Either AdaptError Finding
holeFinding src h = do
  primary <- label src Td.Primary (holeSpan h) ("wanted type:" <+> typeDoc (holeType h))
  pure
    Finding
      { findingKind = HoleKind,
        findingMessage = "typed hole" <+> textDoc (unIdentifier (holeName h)) <> ", wanted type" <+> typeDoc (holeType h),
        findingContext = Td.mkContextDegrading src (primary :| []),
        findingHelp = Just (fitsHeading (holeFits h)),
        findingNotes = map fitDoc (holeFits h)
      }

pipelineFinding :: Td.NamedSource -> Pipeline -> Either AdaptError Finding
pipelineFinding src p = do
  primary <- label src Td.Primary (pipelineSpan p) ("as a whole:" <+> typeDoc (pipelineType p))
  context <- contextWith src primary (pipelineContext p)
  pure
    Finding
      { findingKind = PipelineKind,
        findingMessage = "type flow through a chain joined with" <+> textDoc (operator (pipelineKind p)),
        findingContext = context,
        findingHelp = Just "stage by stage, in the order the data flows",
        findingNotes = stageDocs (pipelineStages p)
      }
  where
    operator :: FlowKind -> Text
    operator = \case
      Composition -> "(.)"
      Application -> "($)"
      Bind -> "(>>=)"

-- | The primary label, plus the signature's span as a secondary label when the
-- finding lies in a binding that has one.
contextWith :: Td.NamedSource -> Td.Labeled Td.Span -> Context -> Either AdaptError Td.Context
contextWith src primary ctx = case ctx of
  InSignedBinding name sp declared -> do
    secondary <- label src Td.Secondary sp (textDoc (unIdentifier name) <+> "is declared as" <+> typeDoc declared)
    pure (Td.mkContextDegrading src (primary :| [secondary]))
  InUnsignedBinding _ -> pure (Td.mkContextDegrading src (primary :| []))
  OutsideBinding -> pure (Td.mkContextDegrading src (primary :| []))

label :: Td.NamedSource -> Td.LabelKind -> Span -> Doc Td.Ann -> Either AdaptError (Td.Labeled Td.Span)
label src kind sp text = do
  (offset, len) <- maybe (Left unusable) Right (offsetsOf (Td.sourceText src) sp)
  tadkaSpan <- either (const (Left unusable)) Right (Td.mkSpan offset len)
  pure (Td.Labeled tadkaSpan kind (Just text))
  where
    unusable :: AdaptError
    unusable =
      UnusableSpan
        (unLine (positionLine (spanStart sp)))
        (unColumn (positionColumn (spanStart sp)))
        (unLine (positionLine (spanEnd sp)))
        (unColumn (positionColumn (spanEnd sp)))

divergenceNote :: Divergence -> [Doc Td.Ann]
divergenceNote d =
  ["GHC could not match" <+> typeDoc (divergenceLeft d) <+> "with" <+> typeDoc (divergenceRight d)]

unsignedNote :: Context -> [Doc Td.Ann]
unsignedNote = \case
  InUnsignedBinding name ->
    [textDoc (unIdentifier name) <+> "has no type signature, so GHC inferred its type; adding one usually moves the error to where you meant it"]
  InSignedBinding {} -> []
  OutsideBinding -> []

fitsHeading :: [HoleFit] -> Doc Td.Ann
fitsHeading = \case
  [] -> "no fits found"
  _ -> "fits, best first"

fitDoc :: HoleFit -> Doc Td.Ann
fitDoc f =
  textDoc (unIdentifier (fitName f) <> " :: " <> unTypeText (fitType f))
    <+> pretty (note (fitLocality f) (fitMatches f))
  where
    note :: Locality -> [TypeText] -> Text
    note locality matches =
      "(" <> localityText locality <> newHoles matches <> ")"
    localityText :: Locality -> Text
    localityText = \case
      Local -> "local"
      Imported -> "imported"
    newHoles :: [TypeText] -> Text
    newHoles [] = ""
    newHoles ms = ", leaves new holes: " <> T.intercalate ", " (map unTypeText ms)

stageDocs :: [Stage] -> [Doc Td.Ann]
stageDocs stages = map stageDoc stages
  where
    width :: Int
    width = maximum (0 : map (T.length . unStageText . stageText) stages)
    stageDoc :: Stage -> Doc Td.Ann
    stageDoc s = fill width (pretty (unStageText (stageText s))) <+> "::" <+> typeDoc (stageType s)

nonEmptyDocs :: [Doc Td.Ann] -> Maybe (Doc Td.Ann)
nonEmptyDocs = \case
  [] -> Nothing
  docs -> Just (vsep docs)

typeDoc :: TypeText -> Doc Td.Ann
typeDoc = textDoc . unTypeText

textDoc :: Text -> Doc Td.Ann
textDoc = annotate Td.AnnCode . pretty
