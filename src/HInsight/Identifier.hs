-- |
-- Module      : HInsight.Identifier
-- Description : A name as written in source.
module HInsight.Identifier
  ( Identifier,
    mkIdentifier,
    unIdentifier,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import HInsight.Error (DomainError (..))

-- | A name as written in source. Invariant: not blank, and trimmed.
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
