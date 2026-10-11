-- |
-- Module      : HInsight
-- Description : Answers questions about Haskell code that need GHC's internals.
--
-- This facade is the public API. The GHC-backed implementation lives in
-- "HInsight.Ghc.Session".
module HInsight
  ( -- * Inputs
    SourceFile,
    mkSourceFile,
    sourceFilePath,
    LibDir,
    mkLibDir,
    libDirPath,

    -- * Results
    Analysis (..),
    emptyAnalysis,
    Insight (..),
    Explanation (..),
    Context (..),
    Pipeline,
    FlowKind (..),
    StageText,
    unStageText,
    Stage (..),
    pipelineSpan,
    pipelineKind,
    pipelineStages,
    pipelineType,
    pipelineContext,
    renderPipeline,
    Identifier,
    mkIdentifier,
    unIdentifier,
    renderExplanation,
    HoleReport (..),
    HoleFit (..),
    rankFits,

    -- * Failures
    InsightError (..),
    renderError,
  )
where

import HInsight.Analysis (Analysis (..), Insight (..), emptyAnalysis)
import HInsight.Config (LibDir, libDirPath, mkLibDir)
import HInsight.Context (Context (..))
import HInsight.Error (InsightError (..), renderError)
import HInsight.Explanation (Explanation (..), renderExplanation)
import HInsight.Flow (FlowKind (..), Pipeline, Stage (..), StageText, pipelineContext, pipelineKind, pipelineSpan, pipelineStages, pipelineType, renderPipeline, unStageText)
import HInsight.Hole (HoleFit (..), HoleReport (..), rankFits)
import HInsight.Identifier (Identifier, mkIdentifier, unIdentifier)
import HInsight.Source (SourceFile, mkSourceFile, sourceFilePath)
