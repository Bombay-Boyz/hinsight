-- |
-- Module      : HInsight.Explanation
-- Description : Why GHC reported a type mismatch, as plain data.
--
-- An 'Explanation' states what GHC expected, what it found, which two
-- sub-types it could not match, and where in the source the mismatch sits.
-- It is built from what GHC records about a mismatch and from the parsed
-- source; it is not a trace of the constraint solver's steps, which GHC does
-- not expose.
module HInsight.Explanation
  ( Divergence,
    mkDivergence,
    divergenceLeft,
    divergenceRight,
    Explanation (..),
    renderExplanation,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import HInsight.Context (Context, renderContext)
import HInsight.Error (DomainError (..))
import HInsight.Source (Span, renderPosition, spanStart)
import HInsight.TypeText (TypeText, unTypeText)

-- | The two sub-types GHC could not match, in the order GHC states them. Which
-- side is the expected one varies, so the order carries no meaning.
-- Invariant: the two sides are different.
data Divergence = UnsafeDivergence !TypeText !TypeText
  deriving stock (Eq, Ord, Show)

-- | Build a 'Divergence'; the two sides must differ.
mkDivergence :: TypeText -> TypeText -> Either DomainError Divergence
mkDivergence l r
  | l == r = Left (IdenticalDivergence (unTypeText l))
  | otherwise = Right (UnsafeDivergence l r)

-- | The first side of a 'Divergence', as GHC states it.
divergenceLeft :: Divergence -> TypeText
divergenceLeft (UnsafeDivergence l _) = l

-- | The second side of a 'Divergence', as GHC states it.
divergenceRight :: Divergence -> TypeText
divergenceRight (UnsafeDivergence _ r) = r

-- | One explained type mismatch.
data Explanation = Explanation
  { explanationSpan :: !Span,
    explanationExpected :: !TypeText,
    explanationActual :: !TypeText,
    explanationDivergence :: !(Maybe Divergence),
    explanationContext :: !Context
  }
  deriving stock (Eq, Show)

-- | Render an explanation as plain lines of text, starting with the position.
-- After the expected and actual types come, when present: the types GHC could
-- not match, and the context line.
renderExplanation :: Explanation -> Text
renderExplanation e =
  T.unlines
    ( [ "type mismatch at " <> renderPosition (spanStart (explanationSpan e)),
        "  expected: " <> unTypeText (explanationExpected e),
        "    actual: " <> unTypeText (explanationActual e)
      ]
        <> foldMap divergenceLines (explanationDivergence e)
        <> renderContext (explanationContext e)
    )
  where
    divergenceLines :: Divergence -> [Text]
    divergenceLines d =
      [ "  could not match: "
          <> unTypeText (divergenceLeft d)
          <> " with "
          <> unTypeText (divergenceRight d)
      ]
