-- |
-- Module      : HInsight.Hole
-- Description : Typed holes and their candidate fits, ranked.
module HInsight.Hole
  ( Identifier,
    mkIdentifier,
    unIdentifier,
    RefinementLevel,
    mkRefinementLevel,
    unRefinementLevel,
    Locality (..),
    HoleFit (..),
    HoleReport (..),
    rankFits,
  )
where

import Data.List (sortOn)
import Data.Text (Text)
import Data.Text qualified as T
import HInsight.Error (DomainError (..))
import HInsight.Explanation (TypeText)
import HInsight.Source (Span)

-- | A name as written in source. Invariant: not blank.
newtype Identifier = UnsafeIdentifier Text
  deriving stock (Eq, Ord, Show)

-- | Build an 'Identifier'; the text must contain a non-space character.
mkIdentifier :: Text -> Either DomainError Identifier
mkIdentifier t
  | T.null (T.strip t) = Left (BlankText "identifier")
  | otherwise = Right (UnsafeIdentifier (T.strip t))

-- | The text inside an 'Identifier'.
unIdentifier :: Identifier -> Text
unIdentifier (UnsafeIdentifier t) = t

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

-- | One candidate that GHC found for a hole.
data HoleFit = HoleFit
  { fitName :: !Identifier,
    fitType :: !TypeText,
    fitRefinement :: !RefinementLevel,
    fitLocality :: !Locality,
    -- | The types the fit's own new holes would have to fill.
    fitMatches :: ![TypeText]
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
    key :: HoleFit -> (RefinementLevel, Locality, Int, Identifier, TypeText, [TypeText])
    key f =
      ( fitRefinement f,
        fitLocality f,
        length (fitMatches f),
        fitName f,
        fitType f,
        fitMatches f
      )
