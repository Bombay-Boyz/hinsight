-- |
-- Module      : HInsight.Refactor.Verify
-- Description : The gate every refactoring plan passes: GHC compiles the result.
--
-- A 'VerifiedPlan' can be built here and nowhere else, so a holder of one
-- knows GHC accepted the text its edits make. What GHC accepted is that the
-- changed file typechecks; the plan does not claim more than that.
module HInsight.Refactor.Verify
  ( Checked (..),
    Checker (..),
    VerifiedPlan,
    planEdits,
    planNotes,
    verifyExtract,
  )
where

import Data.Bifunctor (first)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import HInsight.Edit (EditSet, applyEditSet)
import HInsight.Error (InsightError (..), RefactorError (..))
import HInsight.Identifier (Identifier, mkIdentifier)
import HInsight.Refactor (ExtractFacts (..), FunctionName, planExtract, unFunctionName)
import HInsight.TypeText (TypeText)

-- | What GHC said of a text it was given in place of the file.
data Checked = Checked
  { -- | GHC's error messages, in the order it gave them; none when the text typechecks.
    checkedErrors :: ![Text],
    -- | The type GHC gave each top-level binding of the module.
    checkedTypes :: !(Map Identifier TypeText)
  }
  deriving stock (Eq, Show)

-- | Ask GHC about a text in place of the file's own.
newtype Checker m = Checker
  { checkText :: Text -> m (Either InsightError Checked)
  }

-- | A plan GHC accepted. Only 'verifyExtract' builds one.
data VerifiedPlan = UnsafeVerifiedPlan
  { -- | The edits that make the change.
    planEdits :: !EditSet,
    -- | What a reader should know that the edits do not say.
    planNotes :: ![Text]
  }
  deriving stock (Eq, Show)

-- | Plan the extraction without a signature, have GHC typecheck it, and read
-- the type GHC gave the new function. The plan with that signature is offered
-- when GHC accepts it too; otherwise the plan without one is, with a note.
verifyExtract :: forall m. (Monad m) => Checker m -> Text -> ExtractFacts -> FunctionName -> m (Either InsightError VerifiedPlan)
verifyExtract checker src facts name = case plain of
  Left failure -> pure (Left failure)
  Right (draftSet, draftText) -> do
    drafted <- checkText checker draftText
    case drafted of
      Left failure -> pure (Left failure)
      Right checked -> case checkedErrors checked of
        first' : _ -> pure (Left (RefactorFailure (VerificationFailed first')))
        [] -> case newType checked of
          Left failure -> pure (Left failure)
          Right Nothing -> pure (Right (UnsafeVerifiedPlan draftSet ["GHC gave no type for " <> unFunctionName name <> ", so no signature was added"]))
          Right (Just t) -> signed draftSet t
  where
    plain :: Either InsightError (EditSet, Text)
    plain = planExtract src facts Nothing name >>= withText
    withText :: EditSet -> Either InsightError (EditSet, Text)
    withText set = (,) set <$> applied set
    applied :: EditSet -> Either InsightError Text
    applied set = case first (const (RefactorFailure SelectionNotInText)) (applyEditSet (const (Just src)) set) of
      Left failure -> Left failure
      Right texts -> maybe (Left (RefactorFailure SelectionNotInText)) Right (M.lookup (factsFile facts) texts)
    newType :: Checked -> Either InsightError (Maybe TypeText)
    newType checked = do
      ident <- identifierOf
      pure (M.lookup ident (checkedTypes checked))
    identifierOf :: Either InsightError Identifier
    identifierOf = first DomainFailure (mkIdentifier (unFunctionName name))
    signed :: EditSet -> TypeText -> m (Either InsightError VerifiedPlan)
    signed draftSet t = case planExtract src facts (Just t) name >>= withText of
      Left failure -> pure (Left failure)
      Right (finalSet, finalText) -> do
        again <- checkText checker finalText
        pure $ case again of
          Left failure -> Left failure
          Right checked -> case checkedErrors checked of
            [] -> Right (UnsafeVerifiedPlan finalSet [])
            first' : _ -> Right (UnsafeVerifiedPlan draftSet ["GHC rejected the signature it gave " <> unFunctionName name <> ", so none was added: " <> first'])
