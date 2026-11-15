-- |
-- Module      : HInsight.Refactor.ChangeType
-- Description : The edit that changes a top-level binding's type signature.
--
-- Pure: from where the binding and its signature are, and the type wanted,
-- the edits. Whether GHC accepts the result is 'HInsight.Refactor.Verify'.
module HInsight.Refactor.ChangeType
  ( ChangeFacts (..),
    planChangeType,
  )
where

import Data.Bifunctor (first)
import Data.Text (Text)
import HInsight.Edit (Edit, EditSet, mkEdit, mkEditSet)
import HInsight.Error (InsightError (..))
import HInsight.Identifier (Identifier, valueName)
import HInsight.Source (SourceFile, Span, mkSpan, spanStart)
import HInsight.TypeText (TypeText, unTypeText)

-- | What the parsed module says about the binding.
data ChangeFacts = ChangeFacts
  { changeName :: !Identifier,
    changeFile :: !SourceFile,
    -- | The type of the signature, after the @::@, when there is a signature.
    changeSignature :: !(Maybe Span),
    -- | The binding, from where it starts.
    changeBinding :: !Span
  }
  deriving stock (Eq, Show)

-- | Replace the type of the signature, or put a signature above the binding.
planChangeType :: ChangeFacts -> TypeText -> Either InsightError EditSet
planChangeType facts wanted = do
  edit' <- case changeSignature facts of
    Just sp -> built sp (unTypeText wanted)
    Nothing -> do
      start <- first DomainFailure (mkSpan (spanStart (changeBinding facts)) (spanStart (changeBinding facts)))
      built start (valueName (changeName facts) <> " :: " <> unTypeText wanted <> "\n")
  first DomainFailure (mkEditSet [edit'])
  where
    built :: Span -> Text -> Either InsightError Edit
    built sp t = first DomainFailure (mkEdit (changeFile facts) sp t)
