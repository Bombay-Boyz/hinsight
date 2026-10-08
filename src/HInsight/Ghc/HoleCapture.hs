-- |
-- Module      : HInsight.Ghc.HoleCapture
-- Description : Collect GHC's typed-hole fits through its hole-fit plugin hook.
--
-- GHC 9.10 renders the supplementary part of a hole error (the valid fits)
-- into a document before it stores the diagnostic, so the fits cannot be read
-- back from the error. The hole-fit plugin hook is called with the structured
-- fits first; this module records what it sees.
--
-- A mutable cell is unavoidable here: GHC calls the plugin from inside its own
-- monad, so the only way to get data out is a reference the caller created.
-- The cell is created per session and never shared (recorded in
-- @docs/decisions.md@).
module HInsight.Ghc.HoleCapture
  ( CapturedFit (..),
    CapturedHole (..),
    holePlugin,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, modifyIORef')
import Data.Maybe (mapMaybe)
import GHC.Tc.Errors.Hole.FitTypes (HoleFit (..), TypedHole (..), hfIsLcl)
import qualified GHC.Tc.Errors.Hole.Plugin as Hole
import GHC.Tc.Types (TcM)
import GHC.Tc.Types.Constraint (Hole (..), ctLocSpan)
import GHC.Tc.Utils.Monad (newTcRef)
import GHC.Types.Name (getOccName, occNameString)
import GHC.Types.Name.Reader (rdrNameOcc)
import GHC.Types.SrcLoc (RealSrcSpan)
import HInsight.Ghc.Convert (renderGhcType)

-- | One candidate fit, already rendered so no GHC structure is retained.
data CapturedFit = CapturedFit
  { capturedFitName :: !String,
    capturedFitType :: !String,
    capturedFitRefinement :: !Int,
    capturedFitLocal :: !Bool,
    capturedFitMatches :: ![String]
  }
  deriving stock (Eq, Show)

-- | One hole and the fits GHC found for it.
data CapturedHole = CapturedHole
  { capturedHoleName :: !String,
    capturedHoleType :: !String,
    capturedHoleSpan :: !RealSrcSpan,
    capturedHoleFits :: ![CapturedFit]
  }

-- | A hole-fit plugin that appends what GHC finds to the given cell and
-- returns the fits unchanged, so GHC's own output is not altered.
holePlugin :: IORef [CapturedHole] -> [String] -> Maybe Hole.HoleFitPluginR
holePlugin sink _arguments =
  Just
    Hole.HoleFitPluginR
      { Hole.hfPluginInit = newTcRef (),
        Hole.hfPluginRun =
          const
            Hole.HoleFitPlugin
              { Hole.candPlugin = \_ candidates -> pure candidates,
                Hole.fitPlugin = record sink
              },
        Hole.hfPluginStop = \_ -> pure ()
      }

record :: IORef [CapturedHole] -> TypedHole -> [HoleFit] -> TcM [HoleFit]
record sink typed fits = do
  liftIO (modifyIORef' sink (maybe id (:) (capture typed fits)))
  pure fits

capture :: TypedHole -> [HoleFit] -> Maybe CapturedHole
capture typed fits = describe <$> th_hole typed
  where
    describe :: Hole -> CapturedHole
    describe h =
      CapturedHole
        { capturedHoleName = occNameString (rdrNameOcc (hole_occ h)),
          capturedHoleType = renderGhcType (hole_ty h),
          capturedHoleSpan = ctLocSpan (hole_loc h),
          capturedHoleFits = mapMaybe captureFit fits
        }

-- | Fits GHC itself finds are structured; a 'RawHoleFit' only appears when
-- another plugin has already replaced a fit with a document, and it has no
-- structure to rank, so it is skipped.
captureFit :: HoleFit -> Maybe CapturedFit
captureFit = \case
  fit@HoleFit {hfId = i, hfType = t, hfRefLvl = level, hfMatches = ms} ->
    Just
      CapturedFit
        { capturedFitName = occNameString (getOccName i),
          capturedFitType = renderGhcType t,
          capturedFitRefinement = level,
          capturedFitLocal = hfIsLcl fit,
          capturedFitMatches = map renderGhcType ms
        }
  RawHoleFit _ -> Nothing
