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
    Diagnostic (..),
    MoveChecked (..),
    MoveChecker (..),
    VerifiedPlan,
    planEdits,
    planNotes,
    verifyExtract,
    verifyMove,
  )
where

import Data.Bifunctor (first)
import Data.Containers.ListUtils (nubOrd)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import HInsight.Edit (EditSet, applyEditSet)
import HInsight.Error (InsightError (..), RefactorError (..))
import HInsight.Identifier (Identifier, mkIdentifier)
import HInsight.Refactor (ExtractFacts (..), FunctionName, planExtract, unFunctionName)
import HInsight.Refactor.Move (MoveFacts (..), Pruning (..), copiedImportLines, planMove)
import HInsight.Source (SourceFile, Span, positionLine, renderPosition, sourceFilePath, spanStart, unLine)
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

-- | One message GHC gave about a file, with where it points.
data Diagnostic = Diagnostic
  { diagnosticFile :: !SourceFile,
    diagnosticSpan :: !(Maybe Span),
    diagnosticText :: !Text
  }
  deriving stock (Eq, Show)

-- | What GHC said of several texts given in place of their files.
data MoveChecked = MoveChecked
  { -- | The errors, in every file GHC was asked about.
    moveErrors :: ![Diagnostic],
    -- | The imports GHC called unused.
    moveUnused :: ![Diagnostic]
  }
  deriving stock (Eq, Show)

-- | Ask GHC about texts given in place of files, with the rest of the
-- project, and any files the checker was told to watch, as they are on disk.
newtype MoveChecker m = MoveChecker
  { checkTexts :: Map SourceFile Text -> m (Either InsightError MoveChecked)
  }

-- | Plan the move, have GHC check the result with the imports it copied and
-- kept, then leave out the imports GHC called unused and are not needed, and
-- check again. An import is left out only when GHC still accepts the files
-- without it, and never when it was unused before the move. Errors GHC
-- gives in other files, which the move leaves to be fixed, are notes.
verifyMove :: forall m. (Monad m) => MoveChecker m -> Text -> Text -> MoveFacts -> m (Either InsightError VerifiedPlan)
verifyMove checker fromSrc toSrc facts = do
  baseline <- checkTexts checker original
  case baseline of
    Left failure -> pure (Left failure)
    Right before -> case filter ours (moveErrors before) of
      _ : _ -> pure (Left (RefactorFailure (FileDoesNotCompile (length (filter ours (moveErrors before))))))
      [] -> do
        first' <- trial mempty
        case first' of
          Left failure -> pure (Left failure)
          Right (set0, checked0) -> case filter ours (moveErrors checked0) of
            bad : _ -> pure (Left (RefactorFailure (VerificationFailed (diagnosticText bad))))
            [] -> case candidates before checked0 of
              Left failure -> pure (Left failure)
              Right cands -> do
                settled <- settle (mempty, set0, checked0) cands
                pure $ settled >>= \(_, set, checked) -> Right (UnsafeVerifiedPlan set (pending before checked))
  where
    original :: Map SourceFile Text
    original = M.fromList [(moveFrom facts, fromSrc), (moveTo facts, toSrc)]
    ours :: Diagnostic -> Bool
    ours d = diagnosticFile d == moveFrom facts || diagnosticFile d == moveTo facts
    accepted :: MoveChecked -> Bool
    accepted = not . any ours . moveErrors
    trial :: Pruning -> m (Either InsightError (EditSet, MoveChecked))
    trial pruning = case planMove pruning fromSrc toSrc facts >>= applied of
      Left failure -> pure (Left failure)
      Right (set, texts) -> fmap (set,) <$> checkTexts checker texts
    applied :: EditSet -> Either InsightError (EditSet, Map SourceFile Text)
    applied set = case applyEditSet (`M.lookup` original) set of
      Left _ -> Left (RefactorFailure SelectionNotInText)
      Right changed -> Right (set, M.union changed original)
    lineOf :: Diagnostic -> Maybe Int
    lineOf d = unLine . positionLine . spanStart <$> diagnosticSpan d
    candidates :: MoveChecked -> MoveChecked -> Either InsightError [Pruning]
    candidates before moved = do
      layout <- copiedImportLines mempty fromSrc toSrc facts
      let unusedBefore = [n | d <- moveUnused before, diagnosticFile d == moveFrom facts, Just n <- [lineOf d]]
          copied = [Pruning [sp] [] | d <- moveUnused moved, diagnosticFile d == moveTo facts, Just n <- [lineOf d], (sp, l) <- layout, l == n]
          source =
            [ Pruning [] [sp]
            | d <- moveUnused moved,
              diagnosticFile d == moveFrom facts,
              Just n <- [lineOf d],
              n `notElem` unusedBefore,
              sp <- moveFromImports facts,
              unLine (positionLine (spanStart sp)) == n
            ]
      pure (nubOrd (copied <> source))
    settle :: (Pruning, EditSet, MoveChecked) -> [Pruning] -> m (Either InsightError (Pruning, EditSet, MoveChecked))
    settle start [] = pure (Right start)
    settle start cands = do
      whole <- trial (mconcat cands)
      case whole of
        Left failure -> pure (Left failure)
        Right (set, checked)
          | accepted checked -> pure (Right (mconcat cands, set, checked))
          | otherwise -> oneByOne start cands
    oneByOne :: (Pruning, EditSet, MoveChecked) -> [Pruning] -> m (Either InsightError (Pruning, EditSet, MoveChecked))
    oneByOne state [] = pure (Right state)
    oneByOne state@(pruned, _, _) (c : cs) = do
      tried <- trial (pruned <> c)
      case tried of
        Left failure -> pure (Left failure)
        Right (set, checked)
          | accepted checked -> oneByOne (pruned <> c, set, checked) cs
          | otherwise -> oneByOne state cs
    pending :: MoveChecked -> MoveChecked -> [Text]
    pending before after =
      [ "GHC reports a place still to fix: " <> place d <> diagnosticText d
      | d <- moveErrors after,
        not (ours d),
        d `notElem` moveErrors before
      ]
    place :: Diagnostic -> Text
    place d = T.pack (sourceFilePath (diagnosticFile d)) <> maybe "" (\sp -> ":" <> renderPosition (spanStart sp)) (diagnosticSpan d) <> ": "
