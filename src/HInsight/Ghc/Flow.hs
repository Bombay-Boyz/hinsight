-- |
-- Module      : HInsight.Ghc.Flow
-- Description : Find chains of @.@, @$@ and @>>=@ in the typechecked program.
--
-- GHC rewrites an infix application @a op b@ while typechecking it into the
-- prefix form @(op) a b@ and keeps the original in an expansion node. This
-- module recognises those nodes when @op@ is the standard @(.)@, @($)@ or
-- @(>>=)@ from base, flattens nested uses of the same operator into one
-- chain, and reads each stage's type with 'hsExprType'.
--
-- Limitations of this version: only infix uses are found (not @(.) f g@ or
-- backticks), only these three operators are known, and the program must
-- typecheck, since the types come from the typechecked tree.
module HInsight.Ghc.Flow
  ( RawPipeline,
    collect,
    pipelines,
  )
where

import Control.Monad (guard)
import Data.Data (Data, cast, gmapQ)
import Data.List (sortOn)
import Data.Text qualified as T
import GHC (TypecheckedSource)
import GHC.Hs
  ( GhcTc,
    HsExpr (..),
    HsThingRn (..),
    HsWrap (..),
    LHsExpr,
    XXExprGhcTc (..),
  )
import GHC.Hs.Syn.Type (hsExprType)
import GHC.Parser.Annotation (locA)
import GHC.Types.Id (idName)
import GHC.Types.Name (Name, nameModule_maybe, nameOccName)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.SrcLoc (GenLocated (..), RealSrcSpan, SrcSpan (..), unLoc)
import GHC.Unit.Module (moduleName, moduleNameString)
import HInsight.Error (InsightError)
import HInsight.Flow (FlowKind (..), Pipeline, Stage (..), mkPipeline, mkStageText, pipelineKind, pipelineSpan)
import HInsight.Ghc.Bindings (Binding, contextAt)
import HInsight.Ghc.Convert (convertSpan, convertText, fromDomain, renderUser)
import HInsight.Source (Position, spanStart)

-- | One stage as GHC rendered it: the source text and the type.
data RawStage = RawStage
  { rawStageText :: String,
    rawStageType :: String
  }

-- | One chain as read from the typechecked tree, before conversion. The
-- strings are lazy: they are only checked, and any failure of GHC's printer
-- surfaces, when 'pipelines' converts them.
data RawPipeline = RawPipeline
  { rawSpan :: !RealSrcSpan,
    rawKind :: !FlowKind,
    -- | In the order data flows through them.
    rawStages :: [RawStage],
    rawWhole :: String
  }

-- | Every chain in the typechecked bindings, outermost first.
collect :: TypecheckedSource -> [RawPipeline]
collect = walk

-- | Visit the children of any node, handling each expression we meet.
walk :: forall a. (Data a) => a -> [RawPipeline]
walk x = case cast x of
  Just e -> visit e
  Nothing -> concat (gmapQ walk x)

visit :: LHsExpr GhcTc -> [RawPipeline]
visit le@(L loc e) = case chainOf e of
  Just (kind, _, _) ->
    [RawPipeline rs kind (map rawStage (flow kind stages)) (renderUser (hsExprType e)) | RealSrcSpan rs _ <- [locA loc]]
      <> concatMap visit stages
    where
      stages :: [LHsExpr GhcTc]
      stages = flatten kind le
  Nothing -> descend e

-- | Continue below an expression that is not itself a chain. Expansion nodes
-- other than infix applications (for example @do@ blocks) are entered through
-- their typechecked form; the original they carry is renamer output.
descend :: HsExpr GhcTc -> [RawPipeline]
descend e = case peel e of
  XExpr (ExpandedThingTc _ expanded) -> descend expanded
  other -> concat (gmapQ walk other)

rawStage :: LHsExpr GhcTc -> RawStage
rawStage le = RawStage (renderUser (unLoc le)) (renderUser (hsExprType (unLoc le)))

-- | Stages in the order data flows: @f . g . h@ and @f $ g $ x@ run right to
-- left, @m >>= f >>= g@ left to right.
flow :: FlowKind -> [a] -> [a]
flow = \case
  Composition -> reverse
  Application -> reverse
  Bind -> id

-- | The operands of a chain in source order, flattening nested uses of the
-- same operator and looking through parentheses.
--
-- @.@ is associative, so both sides are flattened. @$@ is right-associative
-- and @>>=@ left-associative, so only the side that continues the chain is.
flatten :: FlowKind -> LHsExpr GhcTc -> [LHsExpr GhcTc]
flatten kind le = case chainOf (unLoc inner) of
  Just (k, l, r)
    | k == kind -> case kind of
        Composition -> flatten kind l <> flatten kind r
        Application -> l : flatten kind r
        Bind -> flatten kind l <> [r]
  _ -> [le]
  where
    inner :: LHsExpr GhcTc
    inner = unparen le

unparen :: LHsExpr GhcTc -> LHsExpr GhcTc
unparen le = case unLoc le of
  HsPar _ inner -> unparen inner
  _ -> le

-- | A source-level infix application of one of the known operators.
chainOf :: HsExpr GhcTc -> Maybe (FlowKind, LHsExpr GhcTc, LHsExpr GhcTc)
chainOf e = case peel e of
  XExpr (ExpandedThingTc (OrigExpr (OpApp {})) expanded) -> do
    (op, l, r) <- operands expanded
    kind <- flowKind op
    Just (kind, l, r)
  _ -> Nothing

-- | The operator and the two operands of @(op) a b@.
operands :: HsExpr GhcTc -> Maybe (Name, LHsExpr GhcTc, LHsExpr GhcTc)
operands e = case peel e of
  HsApp _ f rhs
    | HsApp _ g lhs <- peel (unLoc f),
      HsVar _ (L _ op) <- peel (unLoc g) ->
        Just (idName op, lhs, rhs)
  _ -> Nothing

-- | Remove the type and evidence wrappers GHC puts around an expression.
peel :: HsExpr GhcTc -> HsExpr GhcTc
peel = \case
  XExpr (WrapExpr (HsWrap _ inner)) -> peel inner
  other -> other

-- | The modules that define the standard operators. GHC 9.10 moved the
-- definitions into the @ghc-internal@ package, so they now live in
-- @GHC.Internal.Base@; @GHC.Base@ is kept for older layouts.
baseModules :: [String]
baseModules = ["GHC.Internal.Base", "GHC.Base"]

-- | The known operators, identified by their defining module so that a user's
-- own @(.)@ is not mistaken for the standard one.
flowKind :: Name -> Maybe FlowKind
flowKind name = do
  m <- nameModule_maybe name
  guard (moduleNameString (moduleName m) `elem` baseModules)
  case occNameString (nameOccName name) of
    "." -> Just Composition
    "$" -> Just Application
    ">>=" -> Just Bind
    _ -> Nothing

-- | Convert raw chains into domain values, ordered by position. The first
-- failure to convert stops the whole conversion.
pipelines :: FilePath -> [Binding] -> [RawPipeline] -> Either InsightError [Pipeline]
pipelines file bindings = fmap (sortOn order) . traverse build
  where
    order :: Pipeline -> (Position, FlowKind)
    order p = (spanStart (pipelineSpan p), pipelineKind p)
    build :: RawPipeline -> Either InsightError Pipeline
    build raw = do
      sp <- convertSpan file (rawSpan raw)
      ctx <- contextAt file bindings (rawSpan raw)
      stages <- traverse stage (rawStages raw)
      whole <- convertText file (rawWhole raw)
      fromDomain file (mkPipeline sp (rawKind raw) stages whole ctx)
    stage :: RawStage -> Either InsightError Stage
    stage s =
      Stage
        <$> fromDomain file (mkStageText (T.pack (rawStageText s)))
        <*> convertText file (rawStageType s)
