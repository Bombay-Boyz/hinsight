-- |
-- Module      : HInsight.Flow
-- Description : The type at each stage of a pipeline of @.@, @$@ or @>>=@.
--
-- A 'Pipeline' lists the stages of one chain in the order data flows through
-- them, with the type GHC gave each stage once it had instantiated it. The
-- types are those of the typechecked program, so a pipeline is only available
-- for code that typechecks.
module HInsight.Flow
  ( FlowKind (..),
    StageText,
    mkStageText,
    unStageText,
    Stage (..),
    Pipeline,
    mkPipeline,
    pipelineSpan,
    pipelineKind,
    pipelineStages,
    pipelineType,
    pipelineContext,
    renderPipeline,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import HInsight.Context (Context, renderContext)
import HInsight.Error (DomainError (..))
import HInsight.Source (Span, renderPosition, spanStart)
import HInsight.TypeText (TypeText, unTypeText)

-- | The operator joining a chain. Closed by design (Standard 0.2).
data FlowKind
  = -- | Function composition, @f . g . h@.
    Composition
  | -- | Application, @f $ g $ x@.
    Application
  | -- | Monadic bind, @m >>= f >>= g@.
    Bind
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | The source text of one stage. Invariants: not blank, on one line, and at
-- most 'maxStageWidth' characters (longer text is cut and ends in @...@).
newtype StageText = UnsafeStageText Text
  deriving stock (Eq, Ord, Show)

maxStageWidth :: Int
maxStageWidth = 40

-- | Build a 'StageText': whitespace runs become one space, and over-long text
-- is shortened. Fails on blank input.
mkStageText :: Text -> Either DomainError StageText
mkStageText t
  | T.null normalised = Left (BlankText "stage")
  | T.length normalised <= maxStageWidth = Right (UnsafeStageText normalised)
  | otherwise = Right (UnsafeStageText (T.take (maxStageWidth - 3) normalised <> "..."))
  where
    normalised :: Text
    normalised = T.unwords (T.words t)

-- | The text inside a 'StageText'.
unStageText :: StageText -> Text
unStageText (UnsafeStageText t) = t

-- | One stage: what was written and the type GHC gave it.
data Stage = Stage
  { stageText :: !StageText,
    stageType :: !TypeText
  }
  deriving stock (Eq, Show)

-- | A chain of at least two stages. Invariant enforced by 'mkPipeline'.
data Pipeline = UnsafePipeline
  { pipelineSpan :: !Span,
    pipelineKind :: !FlowKind,
    -- | In the order data flows through them.
    pipelineStages :: ![Stage],
    -- | The type of the whole chain.
    pipelineType :: !TypeText,
    pipelineContext :: !Context
  }
  deriving stock (Eq, Show)

-- | Build a 'Pipeline'; it needs at least two stages.
mkPipeline :: Span -> FlowKind -> [Stage] -> TypeText -> Context -> Either DomainError Pipeline
mkPipeline sp kind stages whole ctx
  | n < 2 = Left (TooFewStages n)
  | otherwise = Right (UnsafePipeline sp kind stages whole ctx)
  where
    n :: Int
    n = length stages

-- | Render a pipeline: a header with the position and operator, one line per
-- stage with its type, the type of the whole, and the context line if any.
renderPipeline :: Pipeline -> Text
renderPipeline p =
  T.unlines
    ( ("type flow at " <> renderPosition (spanStart (pipelineSpan p)) <> ": " <> kindLabel (pipelineKind p))
        : map stageLine (pipelineStages p)
          <> ["  as a whole: " <> unTypeText (pipelineType p)]
          <> renderContext (pipelineContext p)
    )
  where
    width :: Int
    width = maximum (0 : map (T.length . unStageText . stageText) (pipelineStages p))
    stageLine :: Stage -> Text
    stageLine s =
      "  " <> T.justifyLeft width ' ' (unStageText (stageText s)) <> " :: " <> unTypeText (stageType s)

kindLabel :: FlowKind -> Text
kindLabel = \case
  Composition -> "composition with (.)"
  Application -> "application with ($)"
  Bind -> "bind with (>>=)"
