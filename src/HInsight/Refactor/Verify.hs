-- |
-- Module      : HInsight.Refactor.Verify
-- Description : The gate every refactoring plan passes: GHC compiles the result.
--
-- A 'VerifiedPlan' can be built here and nowhere else, so a holder of one
-- knows GHC accepted the text its edits make. What GHC accepted is that the
-- changed file typechecks; the plan does not claim more than that.
module HInsight.Refactor.Verify
  ( Checked (..),
    ChangeChecker (..),
    Checker (..),
    Diagnostic (..),
    MoveChecked (..),
    MoveChecker (..),
    VerifiedPlan,
    planEdits,
    planNotes,
    verifyChangeType,
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
import HInsight.Refactor.ChangeType (ChangeFacts (..), planChangeType)
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

-- | A plan GHC accepted. Only this module builds one: 'verifyExtract',
-- 'verifyMove' and 'verifyChangeType'.
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
verifyExtract :: (Monad m) => Checker m -> Text -> ExtractFacts -> FunctionName -> m (Either InsightError VerifiedPlan)
verifyExtract checker src facts name = case extractText src facts name Nothing of
  Left failure -> pure (Left failure)
  Right (draftSet, draftText) -> checkText checker draftText >>= either (pure . Left) (checkedDraft checker src facts name draftSet)

-- | The plan for the extraction, with the text it makes.
extractText :: Text -> ExtractFacts -> FunctionName -> Maybe TypeText -> Either InsightError (EditSet, Text)
extractText src facts name signature = do
  set <- planExtract src facts signature name
  case first (const (RefactorFailure SelectionNotInText)) (applyEditSet (const (Just src)) set) of
    Left failure -> Left failure
    Right texts -> maybe (Left (RefactorFailure SelectionNotInText)) (Right . (,) set) (M.lookup (factsFile facts) texts)

-- | What GHC gave a binding of this name, when it typechecked the text.
newType :: FunctionName -> Checked -> Either InsightError (Maybe TypeText)
newType name checked = do
  ident <- first DomainFailure (mkIdentifier (unFunctionName name))
  pure (M.lookup ident (checkedTypes checked))

-- | GHC's answer for the plan without a signature: an error refuses it; a
-- type gives the plan a signature to try.
checkedDraft :: (Monad m) => Checker m -> Text -> ExtractFacts -> FunctionName -> EditSet -> Checked -> m (Either InsightError VerifiedPlan)
checkedDraft checker src facts name draftSet checked = case checkedErrors checked of
  first' : _ -> pure (Left (RefactorFailure (VerificationFailed first')))
  [] -> case newType name checked of
    Left failure -> pure (Left failure)
    Right Nothing -> pure (Right (UnsafeVerifiedPlan draftSet ["GHC gave no type for " <> unFunctionName name <> ", so no signature was added"]))
    Right (Just t) -> signedPlan checker src facts name draftSet t

-- | The plan with the signature, when GHC accepts it; else the plan without.
signedPlan :: (Monad m) => Checker m -> Text -> ExtractFacts -> FunctionName -> EditSet -> TypeText -> m (Either InsightError VerifiedPlan)
signedPlan checker src facts name draftSet t = case extractText src facts name (Just t) of
  Left failure -> pure (Left failure)
  Right (finalSet, finalText) -> verdict finalSet <$> checkText checker finalText
  where
    verdict :: EditSet -> Either InsightError Checked -> Either InsightError VerifiedPlan
    verdict finalSet = \case
      Left failure -> Left failure
      Right checked -> Right $ case checkedErrors checked of
        [] -> UnsafeVerifiedPlan finalSet []
        first' : _ -> UnsafeVerifiedPlan draftSet ["GHC rejected the signature it gave " <> unFunctionName name <> ", so none was added: " <> first']

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

-- | What the gate of a move reads: the checker, both texts and the facts.
data Gate m = Gate
  { gateChecker :: MoveChecker m,
    gateFrom :: !Text,
    gateTo :: !Text,
    gateFacts :: !MoveFacts
  }

-- | Plan the move, have GHC check the result with the imports it copied and
-- kept, then leave out the imports GHC called unused and are not needed, and
-- check again. An import is left out only when GHC still accepts the files
-- without it, and never when it was unused before the move. Errors GHC
-- gives in other files, which the move leaves to be fixed, are notes.
verifyMove :: (Monad m) => MoveChecker m -> Text -> Text -> MoveFacts -> m (Either InsightError VerifiedPlan)
verifyMove checker fromSrc toSrc facts = do
  baseline <- checkTexts checker (originalTexts gate)
  case baseline of
    Left failure -> pure (Left failure)
    Right before -> case filter (ours gate) (moveErrors before) of
      [] -> acceptMove gate before
      errors -> pure (Left (RefactorFailure (FileDoesNotCompile (length errors))))
  where
    gate = Gate checker fromSrc toSrc facts

-- | The plan when the files compiled before the move: the move without
-- pruning, then the pruning GHC agrees to.
acceptMove :: (Monad m) => Gate m -> MoveChecked -> m (Either InsightError VerifiedPlan)
acceptMove gate before = do
  first' <- trial gate mempty
  case first' of
    Left failure -> pure (Left failure)
    Right (set0, checked0) -> case filter (ours gate) (moveErrors checked0) of
      bad : _ -> pure (Left (RefactorFailure (VerificationFailed (diagnosticText bad))))
      [] -> case candidates gate before checked0 of
        Left failure -> pure (Left failure)
        Right cands -> do
          settled <- settle gate (mempty, set0, checked0) cands
          pure (settled >>= \(_, set, checked) -> Right (UnsafeVerifiedPlan set (pending gate before checked)))

-- | The two texts as they are.
originalTexts :: Gate m -> Map SourceFile Text
originalTexts gate = M.fromList [(moveFrom (gateFacts gate), gateFrom gate), (moveTo (gateFacts gate), gateTo gate)]

-- | Whether a message is about one of the two files.
ours :: Gate m -> Diagnostic -> Bool
ours gate d = diagnosticFile d `elem` [moveFrom (gateFacts gate), moveTo (gateFacts gate)]

-- | Whether GHC accepted both files.
accepted :: Gate m -> MoveChecked -> Bool
accepted gate = not . any (ours gate) . moveErrors

-- | The plan with this pruning, and what GHC says of the files it makes.
trial :: (Monad m) => Gate m -> Pruning -> m (Either InsightError (EditSet, MoveChecked))
trial gate pruning = case planMove pruning (gateFrom gate) (gateTo gate) (gateFacts gate) >>= applied of
  Left failure -> pure (Left failure)
  Right (set, texts) -> fmap (set,) <$> checkTexts (gateChecker gate) texts
  where
    applied :: EditSet -> Either InsightError (EditSet, Map SourceFile Text)
    applied set = case applyEditSet (`M.lookup` originalTexts gate) set of
      Left _ -> Left (RefactorFailure SelectionNotInText)
      Right changed -> Right (set, M.union changed (originalTexts gate))

lineOf :: Diagnostic -> Maybe Int
lineOf d = unLine . positionLine . spanStart <$> diagnosticSpan d

-- | The prunings GHC's unused-import messages point at: copied imports by
-- their place in the target, source imports by theirs in the source, and
-- never one that was unused before the move.
candidates :: Gate m -> MoveChecked -> MoveChecked -> Either InsightError [Pruning]
candidates gate before moved = do
  layout <- copiedImportLines mempty (gateFrom gate) (gateTo gate) facts
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
  where
    facts :: MoveFacts
    facts = gateFacts gate

-- | All the candidates together when GHC accepts that, else one by one.
settle :: (Monad m) => Gate m -> (Pruning, EditSet, MoveChecked) -> [Pruning] -> m (Either InsightError (Pruning, EditSet, MoveChecked))
settle _ start [] = pure (Right start)
settle gate start cands = do
  whole <- trial gate (mconcat cands)
  case whole of
    Left failure -> pure (Left failure)
    Right (set, checked)
      | accepted gate checked -> pure (Right (mconcat cands, set, checked))
      | otherwise -> oneByOne gate start cands

-- | Each candidate in turn, kept when GHC still accepts the files with it.
oneByOne :: (Monad m) => Gate m -> (Pruning, EditSet, MoveChecked) -> [Pruning] -> m (Either InsightError (Pruning, EditSet, MoveChecked))
oneByOne _ state [] = pure (Right state)
oneByOne gate state@(pruned, _, _) (c : cs) = do
  tried <- trial gate (pruned <> c)
  case tried of
    Left failure -> pure (Left failure)
    Right (set, checked)
      | accepted gate checked -> oneByOne gate (pruned <> c, set, checked) cs
      | otherwise -> oneByOne gate state cs

-- | The errors GHC gives in other files that it did not give before.
pending :: Gate m -> MoveChecked -> MoveChecked -> [Text]
pending gate before after =
  [ "GHC reports a place still to fix: " <> place d <> diagnosticText d
  | d <- moveErrors after,
    not (ours gate d),
    d `notElem` moveErrors before
  ]

-- | Where a message points, as @file:line:column: @.
place :: Diagnostic -> Text
place d = T.pack (sourceFilePath (diagnosticFile d)) <> maybe "" (\sp -> ":" <> renderPosition (spanStart sp)) (diagnosticSpan d) <> ": "

-- | Ask GHC about a text in place of the file's own, twice over: as it is,
-- where the first error stops it, and with type errors deferred, where it
-- reaches every module and gives each as a message.
data ChangeChecker m = ChangeChecker
  { checkStrictly :: Map SourceFile Text -> m (Either InsightError MoveChecked),
    -- | Deferred type errors, typed holes and out-of-scope names come back as errors.
    checkDeferring :: Map SourceFile Text -> m (Either InsightError MoveChecked)
  }

-- | Plan the change, and have GHC check it. The file, and the other files
-- checked with it, must typecheck as they are.
-- An error GHC gives in the changed file refuses the change; an error it
-- gives in another file is a note, a place the change leaves to be fixed.
verifyChangeType :: forall m. (Monad m) => ChangeChecker m -> Text -> ChangeFacts -> TypeText -> m (Either InsightError VerifiedPlan)
verifyChangeType checker src facts wanted = do
  baseline <- checkStrictly checker original
  case baseline of
    Left failure -> pure (Left failure)
    Right before -> case moveErrors before of
      [] -> case planChangeType facts wanted >>= applied of
        Left failure -> pure (Left failure)
        Right (set, texts) -> do
          after <- checkDeferring checker texts
          pure $
            after >>= \checked -> case filter inFile (moveErrors checked) of
              bad : _ -> Left (RefactorFailure (VerificationFailed (diagnosticText bad)))
              [] -> Right (UnsafeVerifiedPlan set [placeNote d | d <- moveErrors checked])
      errors -> pure (Left (RefactorFailure (FileDoesNotCompile (length errors))))
  where
    file :: SourceFile
    file = changeFile facts
    original :: Map SourceFile Text
    original = M.singleton file src
    inFile :: Diagnostic -> Bool
    inFile d = diagnosticFile d == file
    applied :: EditSet -> Either InsightError (EditSet, Map SourceFile Text)
    applied set = case applyEditSet (`M.lookup` original) set of
      Left _ -> Left (RefactorFailure SelectionNotInText)
      Right changed -> Right (set, M.union changed original)
    placeNote :: Diagnostic -> Text
    placeNote d = "GHC reports a place still to fix: " <> place d <> diagnosticText d
