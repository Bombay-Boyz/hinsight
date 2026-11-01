-- |
-- Module      : HInsight.Hole
-- Description : Typed holes and their candidate fits, ranked.
module HInsight.Hole
  ( RefinementLevel,
    mkRefinementLevel,
    unRefinementLevel,
    Locality (..),
    ModuleAlias,
    mkModuleAlias,
    unModuleAlias,
    HoleFit (..),
    HoleReport (..),
    rankFits,
    acceptFit,
  )
where

import Data.List (sortOn)
import Data.Text (Text)
import Data.Text qualified as T
import HInsight.Edit (Edit, mkEdit)
import HInsight.Error (DomainError (..))
import HInsight.Identifier (Identifier, unIdentifier)
import HInsight.Source (SourceFile, Span)
import HInsight.TypeText (TypeText)

-- | How many further holes a fit introduces. Zero means the fit fills the hole
-- on its own. Invariant: not negative.
newtype RefinementLevel = UnsafeRefinementLevel Int
  deriving stock (Eq, Ord, Show)

-- | Build a 'RefinementLevel'; the number must not be negative.
mkRefinementLevel :: Int -> Either DomainError RefinementLevel
mkRefinementLevel n
  | n >= 0 = Right (UnsafeRefinementLevel n)
  | otherwise = Left (NegativeRefinement n)

-- | The number inside a 'RefinementLevel'.
unRefinementLevel :: RefinementLevel -> Int
unRefinementLevel (UnsafeRefinementLevel n) = n

-- | Whether a candidate is bound in the enclosing scope or imported.
--
-- Closed by design (Standard 0.2): adding a case is a compile error in
-- every ranking site. The constructor order is the ranking order.
data Locality = Local | Imported
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | The alias a fit's module is imported under. Invariant: not blank.
newtype ModuleAlias = UnsafeModuleAlias Text
  deriving stock (Eq, Ord, Show)

-- | Build a 'ModuleAlias'; the text must not be blank.
mkModuleAlias :: Text -> Either DomainError ModuleAlias
mkModuleAlias t
  | T.null (T.strip t) = Left (BlankText "module alias")
  | otherwise = Right (UnsafeModuleAlias (T.strip t))

-- | The text inside a 'ModuleAlias'.
unModuleAlias :: ModuleAlias -> Text
unModuleAlias (UnsafeModuleAlias t) = t

-- | One candidate that GHC found for a hole.
data HoleFit = HoleFit
  { fitName :: !Identifier,
    fitType :: !TypeText,
    fitRefinement :: !RefinementLevel,
    fitLocality :: !Locality,
    -- | The types the fit's own new holes would have to fill.
    fitMatches :: ![TypeText],
    -- | The alias the fit is in scope under, when GHC's import data says it
    -- is imported only qualified.
    fitQualifier :: !(Maybe ModuleAlias),
    -- | Whether GHC counts the name as an operator symbol.
    fitIsOperator :: !Bool
  }
  deriving stock (Eq, Ord, Show)

-- | A typed hole together with its ranked fits.
data HoleReport = HoleReport
  { holeName :: !Identifier,
    holeSpan :: !Span,
    holeType :: !TypeText,
    holeFits :: ![HoleFit]
  }
  deriving stock (Eq, Show)

-- | Order fits best first: fewer new holes, then local before imported, then
-- fewer new hole types, then by name and type.
--
-- The sort key contains every field of the fit, so it is injective and the
-- result does not depend on the input order. 'sortOn' is a stable merge sort,
-- O(n log n).
rankFits :: [HoleFit] -> [HoleFit]
rankFits = sortOn key
  where
    key :: HoleFit -> (RefinementLevel, Locality, Int, Identifier, TypeText, [TypeText], Maybe ModuleAlias, Bool)
    key f =
      ( fitRefinement f,
        fitLocality f,
        length (fitMatches f),
        fitName f,
        fitType f,
        fitMatches f,
        fitQualifier f,
        fitIsOperator f
      )

-- | The edit that puts a fit in the place of its hole. A fit that introduces
-- new holes is written as an application to that many holes, in parentheses.
acceptFit :: SourceFile -> HoleReport -> HoleFit -> Either DomainError Edit
acceptFit file hole fit = mkEdit file (holeSpan hole) (renderAcceptance fit)

renderAcceptance :: HoleFit -> Text
renderAcceptance fit = case unRefinementLevel (fitRefinement fit) of
  0 -> spelled
  n -> "(" <> T.unwords (spelled : replicate n "_") <> ")"
  where
    qualified :: Text
    qualified = maybe id (\a name -> unModuleAlias a <> "." <> name) (fitQualifier fit) (unIdentifier (fitName fit))
    spelled :: Text
    spelled = if fitIsOperator fit then "(" <> qualified <> ")" else qualified
