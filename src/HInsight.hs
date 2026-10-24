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
    ProjectGraph,
    ModuleName,
    unModuleName,
    graphModules,
    graphImportCount,
    importsOf,
    dependencyOrder,
    importCycles,
    renderGraph,
    Identifier,
    mkIdentifier,
    unIdentifier,
    renderExplanation,
    UnresolvedInstance (..),
    InstanceText,
    unInstanceText,
    renderUnresolvedInstance,
    Suggested (..),
    Suggestion,
    HintKind (..),
    suggestionKind,
    suggestionText,
    renderSuggested,
    HoleReport (..),
    HoleFit (..),
    rankFits,

    -- * Questions that cost a second pass
    Deep (..),
    ChainView (..),
    Chains (..),
    emptyChains,
    Chain,
    chainSpan,
    chainView,
    chainSteps,
    chainOmitted,
    ChainStep,
    chainStepSpan,
    chainStepSummary,
    chainStepRaw,
    chainStepKindLevel,
    renderChain,
    renderChains,

    -- * Failures
    InsightError (..),
    EnvironmentError (..),
    renderError,
  )
where

import HInsight.Analysis (Analysis (..), Insight (..), emptyAnalysis)
import HInsight.Chain (Chain, ChainStep, ChainView (..), Chains (..), chainOmitted, chainSpan, chainStepKindLevel, chainStepRaw, chainStepSpan, chainStepSummary, chainSteps, chainView, emptyChains, renderChain, renderChains)
import HInsight.Config (LibDir, libDirPath, mkLibDir)
import HInsight.Context (Context (..))
import HInsight.Deep (Deep (..))
import HInsight.Error (EnvironmentError (..), InsightError (..), renderError)
import HInsight.Explanation (Explanation (..), renderExplanation)
import HInsight.Flow (FlowKind (..), Pipeline, Stage (..), StageText, pipelineContext, pipelineKind, pipelineSpan, pipelineStages, pipelineType, renderPipeline, unStageText)
import HInsight.Graph (ModuleName, ProjectGraph, dependencyOrder, graphImportCount, graphModules, importCycles, importsOf, renderGraph, unModuleName)
import HInsight.Hole (HoleFit (..), HoleReport (..), rankFits)
import HInsight.Identifier (Identifier, mkIdentifier, unIdentifier)
import HInsight.Instance (InstanceText, UnresolvedInstance (..), renderUnresolvedInstance, unInstanceText)
import HInsight.Source (SourceFile, mkSourceFile, sourceFilePath)
import HInsight.Suggestion (HintKind (..), Suggested (..), Suggestion, renderSuggested, suggestionKind, suggestionText)
