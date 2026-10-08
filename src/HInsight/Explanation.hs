-- |
-- Module      : HInsight.Explanation
-- Description : Why GHC reported a type mismatch, as plain data.
--
-- An 'Explanation' states what GHC expected, what it found, where the two
-- types first differ, and where the expectation came from. It is built from
-- what GHC records about a mismatch; it is not a trace of the constraint
-- solver's steps, which GHC does not expose.
module HInsight.Explanation
  ( TypeText,
    mkTypeText,
    unTypeText,
    Origin (..),
    Divergence,
    mkDivergence,
    divergenceLeft,
    divergenceRight,
    Explanation (..),
    renderExplanation,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import HInsight.Error (DomainError (..))
import HInsight.Source (Position, Span, positionColumn, positionLine, spanStart, unColumn, unLine)

-- | A type as GHC printed it. Invariants: not blank, and on a single line with
-- single spaces between words, so a rendered explanation has a predictable
-- number of lines.
newtype TypeText = UnsafeTypeText Text
  deriving stock (Eq, Ord, Show)

-- | Build a 'TypeText'. Any run of whitespace, newlines included, becomes one
-- space; the result must contain at least one non-space character.
mkTypeText :: Text -> Either DomainError TypeText
mkTypeText t
  | T.null normalised = Left (BlankText "type")
  | otherwise = Right (UnsafeTypeText normalised)
  where
    normalised :: Text
    normalised = T.unwords (T.words t)

-- | The text inside a 'TypeText'.
unTypeText :: TypeText -> Text
unTypeText (UnsafeTypeText t) = t

-- | Where GHC says the expectation came from, as GHC phrased it.
--
-- Deliberately opaque text: GHC's origin type has dozens of cases and it is
-- not yet known which ones help a reader. The choice is recorded in
-- @docs/decisions.md@ and revisited once real mismatches have been surveyed.
newtype Origin = Origin Text
  deriving stock (Eq, Ord, Show)

-- | The two sub-types at which an expected and an actual type first differ.
-- Invariant: the two sides are different.
data Divergence = UnsafeDivergence !TypeText !TypeText
  deriving stock (Eq, Ord, Show)

-- | Build a 'Divergence'; the two sides must differ.
mkDivergence :: TypeText -> TypeText -> Either DomainError Divergence
mkDivergence l r
  | l == r = Left (IdenticalDivergence (unTypeText l))
  | otherwise = Right (UnsafeDivergence l r)

-- | The expected side of a 'Divergence'.
divergenceLeft :: Divergence -> TypeText
divergenceLeft (UnsafeDivergence l _) = l

-- | The actual side of a 'Divergence'.
divergenceRight :: Divergence -> TypeText
divergenceRight (UnsafeDivergence _ r) = r

-- | One explained type mismatch.
data Explanation = Explanation
  { explanationSpan :: !Span,
    explanationExpected :: !TypeText,
    explanationActual :: !TypeText,
    explanationDivergence :: !(Maybe Divergence),
    explanationOrigin :: !Origin
  }
  deriving stock (Eq, Show)

-- | Render an explanation as plain lines of text, starting with the position.
renderExplanation :: Explanation -> Text
renderExplanation e =
  T.unlines
    ( [ "type mismatch at " <> renderPosition (spanStart (explanationSpan e)),
        "  expected: " <> unTypeText (explanationExpected e),
        "    actual: " <> unTypeText (explanationActual e)
      ]
        <> foldMap divergenceLines (explanationDivergence e)
        <> ["  because: " <> originText (explanationOrigin e)]
    )
  where
    divergenceLines :: Divergence -> [Text]
    divergenceLines d =
      [ "  differs where: "
          <> unTypeText (divergenceLeft d)
          <> " versus "
          <> unTypeText (divergenceRight d)
      ]
    originText :: Origin -> Text
    originText (Origin t) = t

renderPosition :: Position -> Text
renderPosition p = T.pack (show (unLine (positionLine p))) <> ":" <> T.pack (show (unColumn (positionColumn p)))
