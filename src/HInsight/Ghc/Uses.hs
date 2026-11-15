-- |
-- Module      : HInsight.Ghc.Uses
-- Description : How often a module's typechecked program refers to each name.
--
-- A count made by hinsight over what GHC typechecked; GHC does not give one.
module HInsight.Ghc.Uses
  ( UseKey,
    countUses,
    withUses,
  )
where

import Data.Data (Data, cast, gmapQ)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import GHC.Hs (GhcTc, HsExpr (..))
import GHC.Types.Id (idName)
import GHC.Types.Name (nameModule_maybe, nameOccName)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.SrcLoc (GenLocated (..))
import GHC.Unit.Module (moduleName, moduleNameString)
import HInsight.Ghc.HoleCapture (CapturedFit (..), CapturedHole (..))

-- | A name that belongs to a module: the module, and the name in it.
type UseKey = (String, String)

-- | The number of places each module-level name is referred to below a node.
countUses :: (Data a) => a -> Map UseKey Int
countUses x = M.fromListWith (+) (references x)

references :: (Data a) => a -> [(UseKey, Int)]
references x = here <> concat (gmapQ references x)
  where
    here :: [(UseKey, Int)]
    here = case (cast x :: Maybe (HsExpr GhcTc)) of
      Just (HsVar _ (L _ i)) ->
        let n = idName i
         in [((moduleNameString (moduleName m), occNameString (nameOccName n)), 1) | Just m <- [nameModule_maybe n]]
      _ -> []

-- | Put each fit's count in: a fit of a module-level name gets its count,
-- which is none when nothing refers to it; any other fit gets no count.
withUses :: Map UseKey Int -> CapturedHole -> CapturedHole
withUses counts hole = hole {capturedHoleFits = map counted (capturedHoleFits hole)}
  where
    counted :: CapturedFit -> CapturedFit
    counted fit = fit {capturedFitUses = (\key -> M.findWithDefault 0 key counts) <$> capturedFitKey fit}
