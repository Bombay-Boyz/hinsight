-- |
-- Module      : HInsight.Refactor.Move
-- Description : The pure part of moving a top-level definition to another module.
module HInsight.Refactor.Move
  ( Anchor (..),
    ExportClose (..),
    MoveFacts (..),
    Pruning (..),
    planMove,
    copiedImportLines,
    headerEnd,
  )
where

import Control.Monad (unless)
import Data.Bifunctor (first)
import Data.Char (isAlpha, isAlphaNum, isSpace)
import Data.List (findIndices)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import HInsight.Edit (Edit, EditSet, endOfText, mkEdit, mkEditSet, offsetOf)
import HInsight.Error (DomainError, InsightError (..), RefactorError (..))
import HInsight.Graph (ModuleName, unModuleName)
import HInsight.Identifier (Identifier, unIdentifier)
import HInsight.Source
  ( Position,
    SourceFile,
    Span,
    mkColumn,
    mkLine,
    mkPosition,
    mkSpan,
    positionColumn,
    positionLine,
    spanEnd,
    spanStart,
    unColumn,
    unLine,
  )

-- | Where new import lines go in a module.
data Anchor
  = -- | Just past the last import declaration.
    AfterImport !Position
  | -- | Just past the module header, when the module has no imports.
    AfterModuleHeader !Position
  deriving stock (Eq, Show)

-- | The export list of a module.
newtype ExportClose = ExportClose
  { -- | The closing parenthesis, and whether the list holds anything.
    exportClose :: (Position, Bool)
  }
  deriving stock (Eq, Show)

-- | What GHC reported about the two modules and the definition.
data MoveFacts = MoveFacts
  { moveName :: !Identifier,
    moveFrom :: !SourceFile,
    moveTo :: !SourceFile,
    moveToModule :: !ModuleName,
    -- | The signature, when there is one, through the end of the last equation.
    moveSpan :: !Span,
    -- | Every import declaration of the source module, as written.
    moveFromImports :: ![Span],
    moveFromAnchor :: !Anchor,
    -- | Every import declaration of the target module, as written.
    moveToImports :: ![Span],
    moveToAnchor :: !Anchor,
    -- | The target's export list, when it has one.
    moveToExports :: !(Maybe ExportClose),
    -- | Whether the source module still uses the name, or exports it by name.
    moveKeepsImport :: !Bool
  }
  deriving stock (Eq, Show)

-- | Imports to leave out: of the copies made for the target, and of the
-- source module's own. Both name imports by where they are written in the
-- source module.
data Pruning = Pruning
  { pruneCopied :: ![Span],
    pruneSource :: ![Span]
  }
  deriving stock (Eq, Ord, Show)

instance Semigroup Pruning where
  Pruning a b <> Pruning c d = Pruning (a <> c) (b <> d)

instance Monoid Pruning where
  mempty = Pruning [] []

-- | The edits that take the definition out of the source module and put it,
-- with the source's imports minus the ones pruned, in the target.
planMove :: Pruning -> Text -> Text -> MoveFacts -> Either InsightError EditSet
planMove pruning fromSrc toSrc facts = do
  let definition = moveSpan facts
      defLine = unLine (positionLine (spanStart definition))
  unless (unColumn (positionColumn (spanStart definition)) == 1) (refuse NotATopLevelDefinition)
  let firstLine = haddockStart fromLines defLine
  from <- domain (position firstLine 1)
  upTo <- removalEnd
  moved <- refuse' (slice fromSrc =<< domain' (mkSpan' from (spanEnd definition)))
  removal <- edit (moveFrom facts) from upTo ""
  eof <- domain (endOfText toSrc)
  added <- edit (moveTo facts) eof eof ((if "\n" `T.isSuffixOf` toSrc then "\n" else "\n\n") <> moved <> "\n")
  copied <- copiedImports pruning fromSrc toSrc facts
  imports <- anchored (moveTo facts) (moveToAnchor facts) (map snd copied)
  back <- importBack
  pruned <- traverse pruneEdit prunable
  exports <- exportEdit
  domain (mkEditSet (removal : added : imports <> back <> pruned <> exports))
  where
    fromLines :: [Text]
    fromLines = T.splitOn "\n" fromSrc
    refuse :: RefactorError -> Either InsightError a
    refuse = Left . RefactorFailure
    refuse' :: Either RefactorError a -> Either InsightError a
    refuse' = first RefactorFailure
    domain :: Either DomainError a -> Either InsightError a
    domain = first DomainFailure
    domain' :: Either DomainError a -> Either RefactorError a
    domain' = first (const SelectionNotInText)
    position :: Int -> Int -> Either DomainError Position
    position l c = mkPosition <$> mkLine l <*> mkColumn c
    mkSpan' :: Position -> Position -> Either DomainError Span
    mkSpan' = mkSpan
    edit :: SourceFile -> Position -> Position -> Text -> Either InsightError Edit
    edit file a b t = domain (mkSpan a b) >>= \sp -> domain (mkEdit file sp t)
    removalEnd :: Either InsightError Position
    removalEnd
      | endLine >= length fromLines = Right (spanEnd (moveSpan facts))
      | otherwise = domain (position next 1)
      where
        endLine :: Int
        endLine = unLine (positionLine (spanEnd (moveSpan facts)))
        next :: Int
        next = case [n | (n, l) <- zip [1 ..] fromLines, n > endLine, not (T.all isSpace l)] of
          n : _ -> n
          [] -> length fromLines
    importLine :: Text
    importLine = "import " <> unModuleName (moveToModule facts) <> " (" <> nameText <> ")"
    nameText :: Text
    nameText = case T.uncons (unIdentifier (moveName facts)) of
      Just (c, _) | isAlpha c || c == '_' -> unIdentifier (moveName facts)
      _ -> "(" <> unIdentifier (moveName facts) <> ")"
    -- The pruned source imports that sit alone on their lines.
    prunable :: [Span]
    prunable = [sp | sp <- pruneSource pruning, wholeLines sp]
    wholeLines :: Span -> Bool
    wholeLines sp =
      unColumn (positionColumn (spanStart sp)) == 1
        && maybe False (T.all isSpace . T.drop (unColumn (positionColumn (spanEnd sp)) - 1)) (lineAt (unLine (positionLine (spanEnd sp))))
    lineAt :: Int -> Maybe Text
    lineAt n = listToMaybe (drop (n - 1) fromLines)
    pruneEdit :: Span -> Either InsightError Edit
    pruneEdit sp = do
      start <- domain (position (unLine (positionLine (spanStart sp))) 1)
      let endLine = unLine (positionLine (spanEnd sp))
      end <-
        if endLine >= length fromLines
          then Right (spanEnd sp)
          else domain (position (endLine + 1) 1)
      edit (moveFrom facts) start end ""
    -- The import that gives the source the name back. When the import it
    -- would follow is pruned, it takes that import's place.
    importBack :: Either InsightError [Edit]
    importBack
      | not (moveKeepsImport facts) = Right []
      | otherwise = case moveFromAnchor facts of
          AfterImport p
            | (sp : _) <- [sp | sp <- prunable, spanEnd sp == p] -> do
                start <- domain (position (unLine (positionLine (spanStart sp))) 1)
                pure <$> edit (moveFrom facts) start start (importLine <> "\n")
          anchor -> anchored (moveFrom facts) anchor [importLine]
    anchored :: SourceFile -> Anchor -> [Text] -> Either InsightError [Edit]
    anchored _ _ [] = Right []
    anchored file anchor texts = case anchor of
      AfterImport p -> pure <$> edit file p p ("\n" <> T.intercalate "\n" texts)
      AfterModuleHeader p -> pure <$> edit file p p ("\n\n" <> T.intercalate "\n" texts)
    exportEdit :: Either InsightError [Edit]
    exportEdit = case moveToExports facts of
      Just (ExportClose (p, nonEmpty)) | moveKeepsImport facts -> pure <$> edit (moveTo facts) p p ((if nonEmpty then ", " else "") <> nameText)
      _ -> Right []

-- | The source's imports that are copied to the target, each with its text:
-- not the pruned ones, and not those the target already has, whatever their
-- spacing.
copiedImports :: Pruning -> Text -> Text -> MoveFacts -> Either InsightError [(Span, Text)]
copiedImports pruning fromSrc toSrc facts = do
  let wanted = filter (`notElem` pruneCopied pruning) (moveFromImports facts)
  texts <- traverse (first RefactorFailure . slice fromSrc) wanted
  present <- traverse (first RefactorFailure . slice toSrc) (moveToImports facts)
  pure [(sp, t) | (sp, t) <- zip wanted texts, normalised t `notElem` map normalised present]
  where
    normalised :: Text -> Text
    normalised = T.unwords . T.words

-- | Where each copied import starts in the target once the plan is applied:
-- the import as written in the source, and its line in the target's new text.
copiedImportLines :: Pruning -> Text -> Text -> MoveFacts -> Either InsightError [(Span, Int)]
copiedImportLines pruning fromSrc toSrc facts = do
  copied <- copiedImports pruning fromSrc toSrc facts
  let (anchorLine, gap) = case moveToAnchor facts of
        AfterImport p -> (unLine (positionLine p), 1)
        AfterModuleHeader p -> (unLine (positionLine p), 2)
      starts = scanl (+) (anchorLine + gap) [T.count "\n" t + 1 | (_, t) <- copied]
  pure (zip (map fst copied) starts)

slice :: Text -> Span -> Either RefactorError Text
slice src sp = case (offsetOf src (spanStart sp), offsetOf src (spanEnd sp)) of
  (Just a, Just b) | b >= a -> Right (T.take (b - a) (T.drop a src))
  _ -> Left SelectionNotInText

-- | The first line of the Haddock comment directly above a definition, or the
-- definition's own line when there is none.
haddockStart :: [Text] -> Int -> Int
haddockStart ls defLine = case above of
  nearest : _
    | "--" `T.isPrefixOf` nearest -> fromRun (takeWhile (T.isPrefixOf "--") above)
    | "-}" `T.isSuffixOf` T.stripEnd nearest -> fromBlock above
  _ -> defLine
  where
    above :: [Text]
    above = reverse (take (defLine - 1) ls)
    fromRun :: [Text] -> Int
    fromRun run = maybe defLine (\j -> defLine - 1 - j) (listToMaybe (reverse (findIndices (T.isPrefixOf "-- |") run)))
    fromBlock :: [Text] -> Int
    fromBlock block = case [(i, l) | (i, l) <- zip [0 :: Int ..] block, "{-" `T.isPrefixOf` l] of
      (i, l) : _ | "{- |" `T.isPrefixOf` l || "{-|" `T.isPrefixOf` l -> defLine - 1 - i
      _ -> defLine

-- | The position just past the @where@ that ends a module header, found by
-- reading the text from a position inside the header (the end of the module
-- name or of the export list). Spaces, comments and pragmas are skipped;
-- anything else is not a header this can place imports after.
headerEnd :: Text -> Position -> Either RefactorError Position
headerEnd src from = do
  start <- maybe (Left SelectionNotInText) Right (offsetOf src from)
  used <- maybe (Left ModuleHeaderNotRecognised) Right (skipToWhere 0 (T.drop start src))
  first (const ModuleHeaderNotRecognised) (endOfText (T.take (start + used) src))

-- | How many characters come before the end of the first @where@, when only
-- spaces and comments come before it.
skipToWhere :: Int -> Text -> Maybe Int
skipToWhere n t = case T.uncons t of
  Nothing -> Nothing
  Just (c, more)
    | isSpace c -> skipToWhere (n + 1) more
    | "--" `T.isPrefixOf` t -> let (line, rest) = T.break (== '\n') t in skipToWhere (n + T.length line) rest
    | "{-" `T.isPrefixOf` t -> skipBlock (n + 2) (1 :: Int) (T.drop 2 t)
    | Just after <- T.stripPrefix "where" t,
      maybe True (not . isNameChar . fst) (T.uncons after) ->
        Just (n + 5)
    | otherwise -> Nothing
  where
    skipBlock :: Int -> Int -> Text -> Maybe Int
    skipBlock m 0 rest = skipToWhere m rest
    skipBlock m depth rest
      | "-}" `T.isPrefixOf` rest = skipBlock (m + 2) (depth - 1) (T.drop 2 rest)
      | "{-" `T.isPrefixOf` rest = skipBlock (m + 2) (depth + 1) (T.drop 2 rest)
      | otherwise = T.uncons rest >>= \(_, more) -> skipBlock (m + 1) depth more
    isNameChar :: Char -> Bool
    isNameChar ch = isAlphaNum ch || ch `elem` ("_'" :: String)
