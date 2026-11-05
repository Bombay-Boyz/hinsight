-- |
-- Module      : HInsight.Ghc.Refactor
-- Description : What GHC's renamed program says about a selected expression.
module HInsight.Ghc.Refactor
  ( selectionFacts,
  )
where

import Control.Monad (filterM)
import Data.Containers.ListUtils (nubOrd)
import Data.Data (Data, cast, gmapQ)
import Data.List (find, sortOn)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Text qualified as T
import GHC.Hs (GhcRn, HsExpr (..), LHsExpr)
import GHC.Parser.Annotation (locA)
import GHC.Types.Name (Name, isInternalName, nameOccName, nameSrcSpan)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.SrcLoc (GenLocated (..), RealSrcSpan, SrcSpan (..), realSrcSpanStart)
import HInsight.Error (InsightError (..), RefactorError (..))
import HInsight.Ghc.Convert (convertSpan, fromDomain)
import HInsight.Identifier (Identifier, mkIdentifier)
import HInsight.Refactor (CallContext (..), ExtractFacts (..))
import HInsight.Source (SourceFile, Span, sourceFilePath, spanContains)

-- | The facts 'HInsight.Refactor.planExtract' needs, read from the renamed
-- program: the expression that exactly fills the selection, the top-level
-- declaration around it, the local variables it uses that are bound outside
-- it, and what surrounds it.
selectionFacts :: (Data a) => SourceFile -> Span -> [Span] -> a -> Either InsightError ExtractFacts
selectionFacts file selection declarations renamed = do
  declaration <- maybe (refuse SelectionNotInDeclaration) Right (find (`spanContains` selection) declarations)
  (parent, node) <- maybe (refuse SelectionNotAnExpression) Right (locate isSelection Nothing renamed)
  free <- filterM (\(_, n) -> freeIn path selection n) (sortOn (realSrcSpanStart . fst) (occurrences node))
  names <- nubOrd <$> traverse (identifier path . snd) free
  pure (ExtractFacts file selection declaration names (contextOf parent))
  where
    path :: FilePath
    path = sourceFilePath file
    isSelection :: RealSrcSpan -> Bool
    isSelection rs = either (const False) (== selection) (convertSpan path rs)

refuse :: RefactorError -> Either InsightError a
refuse = Left . RefactorFailure

-- | The outermost expression whose span satisfies the test, with the
-- expression that most closely encloses it.
locate :: forall a. (Data a) => (RealSrcSpan -> Bool) -> Maybe (HsExpr GhcRn) -> a -> Maybe (Maybe (HsExpr GhcRn), LHsExpr GhcRn)
locate matches parent x = case (cast x :: Maybe (LHsExpr GhcRn)) of
  Just e@(L loc inner)
    | RealSrcSpan rs _ <- locA loc,
      matches rs ->
        Just (parent, e)
    | otherwise -> below (Just inner)
  Nothing -> below parent
  where
    below :: Maybe (HsExpr GhcRn) -> Maybe (Maybe (HsExpr GhcRn), LHsExpr GhcRn)
    below p = listToMaybe (catMaybes (gmapQ (locate matches p) x))

-- | Every use of a variable below a node, with where it is written.
occurrences :: forall a. (Data a) => a -> [(RealSrcSpan, Name)]
occurrences x = here <> concat (gmapQ occurrences x)
  where
    here :: [(RealSrcSpan, Name)]
    here = case (cast x :: Maybe (HsExpr GhcRn)) of
      Just (HsVar _ (L loc n))
        | RealSrcSpan rs _ <- locA loc -> [(rs, n)]
      _ -> []

-- | Whether a name is a local variable bound outside the selection.
freeIn :: FilePath -> Span -> Name -> Either InsightError Bool
freeIn path selection n
  | not (isInternalName n) = Right False
  | otherwise = case nameSrcSpan n of
      RealSrcSpan binder _ -> not . spanContains selection <$> convertSpan path binder
      _ -> Right True

identifier :: FilePath -> Name -> Either InsightError Identifier
identifier path = fromDomain path . mkIdentifier . T.pack . occNameString . nameOccName

-- | Whether a call that replaces the expression needs parentheses: not when a
-- delimiter or a keyword already encloses it.
contextOf :: Maybe (HsExpr GhcRn) -> CallContext
contextOf = \case
  Nothing -> CallIsWhole
  Just parent
    | delimits parent -> CallIsWhole
    | otherwise -> CallIsPart
  where
    delimits :: HsExpr GhcRn -> Bool
    delimits = \case
      HsPar {} -> True
      HsDo {} -> True
      HsIf {} -> True
      HsMultiIf {} -> True
      HsCase {} -> True
      HsLet {} -> True
      HsLam {} -> True
      ExplicitList {} -> True
      ExplicitTuple {} -> True
      _ -> False
