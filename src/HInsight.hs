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
import HInsight.Error (InsightError (..), renderError)
import HInsight.Explanation (Explanation (..), renderExplanation)
import HInsight.Hole (HoleFit (..), HoleReport (..), rankFits)
import HInsight.Source (SourceFile, mkSourceFile, sourceFilePath)
