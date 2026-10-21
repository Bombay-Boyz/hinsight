-- |
-- Module      : HInsight.Suggestion
-- Description : The hints GHC attaches to a diagnostic, as plain data.
--
-- A 'Suggested' says that GHC attached one or more hints to the error at a
-- span. Each 'Suggestion' has a kind, taken from the kind of hint GHC used,
-- and GHC's own wording of it. Nothing here works out what to suggest; the
-- text is GHC's, and a kind this package does not know is 'OtherHint'.
module HInsight.Suggestion
  ( HintKind (..),
    Suggestion,
    mkSuggestion,
    suggestionKind,
    suggestionText,
    Suggested (..),
    renderSuggested,
  )
where

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Text qualified as T
import HInsight.Error (DomainError (..))
import HInsight.Source (Span, renderPosition, spanStart)

-- | The kind of hint GHC used. Closed by design (Standard 0.2): a kind GHC
-- adds later is 'OtherHint' until it is given a constructor here.
data HintKind
  = -- | Names in scope that are spelled like the one that was not found.
    SimilarNames
  | -- | An import that would bring a name into scope.
    Import
  | -- | A language extension to turn on.
    Extension
  | -- | Type signatures to add.
    Signature
  | -- | A wildcard to put in a constraint.
    WildcardConstraint
  | -- | Parentheses to add.
    Parentheses
  | -- | A missing @do@.
    MissingDo
  | -- | A @let@ inside a @do@.
    LetInDo
  | -- | A binding to a wildcard.
    BindToWildcard
  | -- | Any other kind of hint.
    OtherHint
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | One hint: its kind and GHC's wording. Invariant: the text is not blank and
-- is on a single line with single spaces between words.
data Suggestion = UnsafeSuggestion !HintKind !Text
  deriving stock (Eq, Show)

-- | Build a 'Suggestion'. Any run of whitespace becomes one space; the result
-- must contain at least one non-space character.
mkSuggestion :: HintKind -> Text -> Either DomainError Suggestion
mkSuggestion kind t
  | T.null normalised = Left (BlankText "suggestion")
  | otherwise = Right (UnsafeSuggestion kind normalised)
  where
    normalised :: Text
    normalised = T.unwords (T.words t)

-- | The kind of a 'Suggestion'.
suggestionKind :: Suggestion -> HintKind
suggestionKind (UnsafeSuggestion k _) = k

-- | GHC's wording of a 'Suggestion'.
suggestionText :: Suggestion -> Text
suggestionText (UnsafeSuggestion _ t) = t

-- | The hints GHC attached to the error at a span. There is at least one.
data Suggested = Suggested
  { suggestedSpan :: !Span,
    suggestedHints :: !(NonEmpty Suggestion)
  }
  deriving stock (Eq, Show)

-- | Render as plain lines: a heading with the position, then one line per
-- hint in GHC's order.
renderSuggested :: Suggested -> Text
renderSuggested s =
  T.unlines
    ( ("GHC's suggestions for the error at " <> renderPosition (spanStart (suggestedSpan s)) <> ":")
        : map (("  " <>) . suggestionText) (toList (suggestedHints s))
    )
