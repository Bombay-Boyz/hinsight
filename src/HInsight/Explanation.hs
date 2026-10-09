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
  ( TypeText,
    mkTypeText,
    unTypeText,
    Context (..),
    Divergence,
    mkDivergence,
    divergenceLeft,
    divergenceRight,
    Explanation (..),
    renderExplanation,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import HInsight.Error (DomainError (..))
import HInsight.Identifier (Identifier, unIdentifier)
import HInsight.Source (Span, renderPosition, spanStart)

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

-- | Where in the program a mismatch sits: the top-level binding around it and
-- what that binding declares. This is a statement of fact about the source,
-- not a claim that the declared type caused the mismatch.
--
-- Closed by design (Standard 0.2): adding a case is a compile error at every
-- site that renders one.
data Context
  = -- | Inside a binding that has a type signature: its name, where the
    -- signature starts, and the type the signature declares.
    InSignedBinding !Identifier !Span !TypeText
  | -- | Inside a binding with no type signature, so GHC inferred its type.
    InUnsignedBinding !Identifier
  | -- | Not inside any top-level function or variable binding.
    OutsideBinding
  deriving stock (Eq, Show)

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
        <> contextLines (explanationContext e)
    )
  where
    divergenceLines :: Divergence -> [Text]
    divergenceLines d =
      [ "  could not match: "
          <> unTypeText (divergenceLeft d)
          <> " with "
          <> unTypeText (divergenceRight d)
      ]

contextLines :: Context -> [Text]
contextLines = \case
  InSignedBinding name sp declared ->
    [ "  context: in "
        <> unIdentifier name
        <> ", declared at "
        <> renderPosition (spanStart sp)
        <> " as "
        <> unTypeText declared
    ]
  InUnsignedBinding name ->
    ["  context: in " <> unIdentifier name <> ", which has no type signature"]
  OutsideBinding -> []
