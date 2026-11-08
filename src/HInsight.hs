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
    CoreDump (..),
    CoreSection (..),
    RuleFiring (..),
    Optimisation (..),
    CoreDetail (..),
    CoreRequest (..),
    MissedKind (..),
    MissedWarning (..),
    coreOnly,
    SectionKind (..),
    sectionKind,
    emptyCore,
    renderCore,
    Splices (..),
    emptySplices,
    Splice,
    SpliceKind (..),
    spliceSpan,
    spliceKind,
    spliceSource,
    splicePending,
    spliceResult,
    spliceRaw,
    renderSplice,
    renderSplices,

    -- * Positions
    Position,
    mkPosition,
    positionLine,
    positionColumn,
    Line,
    mkLine,
    unLine,
    Column,
    mkColumn,
    unColumn,
    Span,
    mkSpan,
    spanStart,
    spanEnd,
    spanContains,
    renderPosition,

    -- * Edits
    Edit,
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
    endOfText,
    renderEditError,
    acceptFit,
    ModuleAlias,
    mkModuleAlias,
    unModuleAlias,
    RefinementLevel,
    mkRefinementLevel,
    unRefinementLevel,
    Locality (..),

    -- * Refactoring
    Refactor (..),
    FunctionName,
    mkFunctionName,
    unFunctionName,
    VerifiedPlan,
    planEdits,
    planNotes,

    -- * Failures
    InsightError (..),
    EnvironmentError (..),
    RefactorError (..),
    renderError,
    renderRefactorError,
  )
where

import HInsight.Analysis (Analysis (..), Insight (..), emptyAnalysis)
import HInsight.Chain (Chain, ChainStep, ChainView (..), Chains (..), chainOmitted, chainSpan, chainStepKindLevel, chainStepRaw, chainStepSpan, chainStepSummary, chainSteps, chainView, emptyChains, renderChain, renderChains)
import HInsight.Config (LibDir, libDirPath, mkLibDir)
import HInsight.Context (Context (..))
import HInsight.Core (CoreDetail (..), CoreDump (..), CoreRequest (..), CoreSection (..), MissedKind (..), MissedWarning (..), Optimisation (..), RuleFiring (..), SectionKind (..), coreOnly, emptyCore, renderCore, sectionKind)
import HInsight.Deep (Deep (..))
import HInsight.Edit (Edit, EditError (..), EditSet, applyEditSet, editFile, editSetEdits, editSpan, editText, endOfText, mkEdit, mkEditSet, offsetOf, renderEditError)
import HInsight.Error (EnvironmentError (..), InsightError (..), RefactorError (..), renderError, renderRefactorError)
import HInsight.Explanation (Explanation (..), renderExplanation)
import HInsight.Flow (FlowKind (..), Pipeline, Stage (..), StageText, pipelineContext, pipelineKind, pipelineSpan, pipelineStages, pipelineType, renderPipeline, unStageText)
import HInsight.Graph (ModuleName, ProjectGraph, dependencyOrder, graphImportCount, graphModules, importCycles, importsOf, renderGraph, unModuleName)
import HInsight.Hole (HoleFit (..), HoleReport (..), Locality (..), ModuleAlias, RefinementLevel, acceptFit, mkModuleAlias, mkRefinementLevel, rankFits, unModuleAlias, unRefinementLevel)
import HInsight.Identifier (Identifier, mkIdentifier, unIdentifier)
import HInsight.Instance (InstanceText, UnresolvedInstance (..), renderUnresolvedInstance, unInstanceText)
import HInsight.Refactor (FunctionName, mkFunctionName, unFunctionName)
import HInsight.Refactor.Handle (Refactor (..))
import HInsight.Refactor.Verify (VerifiedPlan, planEdits, planNotes)
import HInsight.Source (Column, Line, Position, SourceFile, Span, mkColumn, mkLine, mkPosition, mkSourceFile, mkSpan, positionColumn, positionLine, renderPosition, sourceFilePath, spanContains, spanEnd, spanStart, unColumn, unLine)
import HInsight.Splice (Splice, SpliceKind (..), Splices (..), emptySplices, renderSplice, renderSplices, spliceKind, splicePending, spliceRaw, spliceResult, spliceSource, spliceSpan)
import HInsight.Suggestion (HintKind (..), Suggested (..), Suggestion, renderSuggested, suggestionKind, suggestionText)
