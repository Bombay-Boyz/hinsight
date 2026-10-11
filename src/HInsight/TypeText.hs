-- |
-- Module      : HInsight.TypeText
-- Description : A type as GHC printed it, validated.
module HInsight.TypeText
  ( TypeText,
    mkTypeText,
    unTypeText,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import HInsight.Error (DomainError (..))

-- | A type as GHC printed it. Invariants: not blank, and on a single line with
-- single spaces between words, so a rendered result has a predictable number
-- of lines.
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
