-- |
-- Module      : HInsight.Ghc.Graph
-- Description : Read the project's module graph from GHC's dependency analysis.
--
-- Uses the module summaries GHC produced while chasing the file's imports.
-- Only ordinary imports are followed: @{-# SOURCE #-}@ imports are left out,
-- because a cycle that an @hs-boot@ file breaks is legal and should not be
-- reported as a cycle. Boot files are not modules of their own here, and
-- imports of modules outside the project are dropped.
module HInsight.Ghc.Graph
  ( RawModule,
    collectGraph,
    projectGraph,
  )
where

import Data.Set qualified as Set
import Data.Text qualified as T
import GHC (ModSummary (..), ModuleGraph, mgModSummaries)
import GHC.Types.SrcLoc (unLoc)
import GHC.Unit.Module (moduleNameString)
import GHC.Unit.Module.ModSummary (isBootSummary, ms_mod_name)
import GHC.Unit.Types (IsBootInterface (..))
import HInsight.Error (InsightError)
import HInsight.Ghc.Convert (fromDomain)
import HInsight.Graph (ModuleName, ProjectGraph, mkModuleName, mkProjectGraph)

-- | A module and the modules it imports, as names GHC printed.
data RawModule = RawModule
  { rawName :: String,
    rawImports :: [String]
  }

-- | The modules of a dependency analysis, with their ordinary imports.
collectGraph :: ModuleGraph -> [RawModule]
collectGraph graph =
  [ RawModule
      (moduleNameString (ms_mod_name s))
      [moduleNameString (unLoc imported) | (_, imported) <- ms_textual_imps s]
  | s <- mgModSummaries graph,
    isBootSummary s == NotBoot
  ]

-- | Convert raw modules into a 'ProjectGraph', keeping only the imports that
-- are themselves modules of the project.
projectGraph :: FilePath -> [RawModule] -> Either InsightError ProjectGraph
projectGraph file raws = do
  entries <- traverse entry raws
  fromDomain file (mkProjectGraph entries)
  where
    local :: Set.Set String
    local = Set.fromList (map rawName raws)
    entry :: RawModule -> Either InsightError (ModuleName, [ModuleName])
    entry raw =
      (,)
        <$> name (rawName raw)
        <*> traverse name (filter (`Set.member` local) (rawImports raw))
    name :: String -> Either InsightError ModuleName
    name = fromDomain file . mkModuleName . T.pack
