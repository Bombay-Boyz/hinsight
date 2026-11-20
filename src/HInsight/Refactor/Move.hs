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
import Data.Bool (bool)
import Data.Char (isAlphaNum, isSpace)
import Data.List (findIndices)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import HInsight.Edit (Edit, EditSet, endOfText, mkEdit, mkEditSet, offsetOf)
import HInsight.Error (DomainError, InsightError (..), RefactorError (..))
import HInsight.Graph (ModuleName, unModuleName)
import HInsight.Identifier (Identifier, valueName)
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

-- | What each step of a plan reads: the pruning, both texts and the facts.
data Ctx = Ctx
  { ctxPruning :: !Pruning,
    ctxFromSrc :: !Text,
    ctxToSrc :: !Text,
    ctxFacts :: !MoveFacts
  }

-- | The edits that take the definition out of the source module and put it,
-- with the source's imports minus the ones pruned, in the target.
planMove :: Pruning -> Text -> Text -> MoveFacts -> Either InsightError EditSet
planMove pruning fromSrc toSrc facts = do
  unless (unColumn (positionColumn (spanStart (moveSpan facts))) == 1) (refuse NotATopLevelDefinition)
  (from, moved) <- movedBlock ctx
  removal <- removalEdit ctx from
  added <- additionEdit ctx moved
  imports <- importEdits ctx
  fixes <- sourceEdits ctx
  exports <- exportEdit ctx
  domain (mkEditSet (removal : added : imports <> fixes <> exports))
  where
    ctx :: Ctx
    ctx = Ctx pruning fromSrc toSrc facts

refuse :: RefactorError -> Either InsightError a
refuse = Left . RefactorFailure

domain :: Either DomainError a -> Either InsightError a
domain = first DomainFailure

positionAt :: Int -> Int -> Either DomainError Position
positionAt l c = mkPosition <$> mkLine l <*> mkColumn c

-- | An edit of a file, between two positions.
editAt :: SourceFile -> Position -> Position -> Text -> Either InsightError Edit
editAt file a b t = domain (mkSpan a b) >>= \sp -> domain (mkEdit file sp t)

fromLines :: Ctx -> [Text]
fromLines = T.splitOn "\n" . ctxFromSrc

eolFrom, eolTo :: Ctx -> Text
eolFrom = lineEnding . ctxFromSrc
eolTo = lineEnding . ctxToSrc

nameText :: Ctx -> Text
nameText = valueName . moveName . ctxFacts

-- | The first position of the moved block, with its Haddock comment, and its text.
movedBlock :: Ctx -> Either InsightError (Position, Text)
movedBlock ctx = do
  let definition = moveSpan (ctxFacts ctx)
  from <- domain (positionAt (haddockStart (fromLines ctx) (unLine (positionLine (spanStart definition)))) 1)
  block <- first RefactorFailure (slice (ctxFromSrc ctx) =<< first (const SelectionNotInText) (mkSpan from (spanEnd definition)))
  pure (from, block)

-- | The edit that takes the block out, with the blank lines after it up to
-- the next text.
removalEdit :: Ctx -> Position -> Either InsightError Edit
removalEdit ctx from = removalEnd >>= \upTo -> editAt (moveFrom (ctxFacts ctx)) from upTo ""
  where
    ls :: [Text]
    ls = fromLines ctx
    endLine :: Int
    endLine = unLine (positionLine (spanEnd (moveSpan (ctxFacts ctx))))
    removalEnd :: Either InsightError Position
    removalEnd
      | endLine >= length ls = Right (spanEnd (moveSpan (ctxFacts ctx)))
      | otherwise = domain (positionAt nextText 1)
    nextText :: Int
    nextText = case [n | (n, l) <- zip [1 ..] ls, n > endLine, not (T.all isSpace l)] of
      n : _ -> n
      [] -> length ls

-- | The edit that puts the block at the end of the target.
additionEdit :: Ctx -> Text -> Either InsightError Edit
additionEdit ctx moved = do
  eof <- domain (endOfText (ctxToSrc ctx))
  let eol = eolTo ctx
      gap = if "\n" `T.isSuffixOf` ctxToSrc ctx then eol else eol <> eol
  editAt (moveTo (ctxFacts ctx)) eof eof (gap <> withEol eol moved <> eol)

-- | The edit that adds the lines at an anchor of a file, in that file's line ending.
anchoredEdits :: Ctx -> SourceFile -> Anchor -> [Text] -> Either InsightError [Edit]
anchoredEdits _ _ _ [] = Right []
anchoredEdits ctx file anchor texts = case anchor of
  AfterImport p -> pure <$> editAt file p p (eol <> T.intercalate eol texts)
  AfterModuleHeader p -> pure <$> editAt file p p (eol <> eol <> T.intercalate eol texts)
  where
    eol :: Text
    eol = if file == moveTo (ctxFacts ctx) then eolTo ctx else eolFrom ctx

-- | The source's imports that the target needs, added to the target.
importEdits :: Ctx -> Either InsightError [Edit]
importEdits ctx = do
  copied <- copiedImports (ctxPruning ctx) (ctxFromSrc ctx) (ctxToSrc ctx) (ctxFacts ctx)
  anchoredEdits ctx (moveTo (ctxFacts ctx)) (moveToAnchor (ctxFacts ctx)) (map snd copied)

-- | The pruned source imports that sit alone on their lines.
prunable :: Ctx -> [Span]
prunable ctx = [sp | sp <- pruneSource (ctxPruning ctx), wholeLines sp]
  where
    wholeLines :: Span -> Bool
    wholeLines sp =
      unColumn (positionColumn (spanStart sp)) == 1
        && maybe False (T.all isSpace . T.drop (unColumn (positionColumn (spanEnd sp)) - 1)) (lineAt (unLine (positionLine (spanEnd sp))))
    lineAt :: Int -> Maybe Text
    lineAt n = listToMaybe (drop (n - 1) (fromLines ctx))

-- | The edits to the source's imports: the pruned ones go, and the import
-- that gives the source the name back comes in.
sourceEdits :: Ctx -> Either InsightError [Edit]
sourceEdits ctx = (<>) <$> importBack ctx <*> traverse (pruneEdit ctx) (prunable ctx)

-- | The line or lines of a pruned import, taken out whole.
pruneEdit :: Ctx -> Span -> Either InsightError Edit
pruneEdit ctx sp = do
  start <- domain (positionAt (unLine (positionLine (spanStart sp))) 1)
  end <- if endLine >= length (fromLines ctx) then Right (spanEnd sp) else domain (positionAt (endLine + 1) 1)
  editAt (moveFrom (ctxFacts ctx)) start end ""
  where
    endLine :: Int
    endLine = unLine (positionLine (spanEnd sp))

-- | The import that gives the source the name back. When the import it
-- would follow is pruned, it takes that import's place.
importBack :: Ctx -> Either InsightError [Edit]
importBack ctx
  | not (moveKeepsImport facts) = Right []
  | otherwise = case moveFromAnchor facts of
      AfterImport p
        | (sp : _) <- [sp | sp <- prunable ctx, spanEnd sp == p] -> do
            start <- domain (positionAt (unLine (positionLine (spanStart sp))) 1)
            pure <$> editAt (moveFrom facts) start start (importLine <> eolFrom ctx)
      anchor -> anchoredEdits ctx (moveFrom facts) anchor [importLine]
  where
    facts :: MoveFacts
    facts = ctxFacts ctx
    importLine :: Text
    importLine = "import " <> unModuleName (moveToModule facts) <> " (" <> nameText ctx <> ")"

-- | The edit that adds the name to the target's export list, when the source keeps importing it.
exportEdit :: Ctx -> Either InsightError [Edit]
exportEdit ctx = case moveToExports facts of
  Just (ExportClose (p, nonEmpty)) | moveKeepsImport facts -> pure <$> editAt (moveTo facts) p p ((if nonEmpty then ", " else "") <> nameText ctx)
  _ -> Right []
  where
    facts :: MoveFacts
    facts = ctxFacts ctx

-- | The line ending a text uses: CRLF when it has one, else LF.
lineEnding :: Text -> Text
lineEnding t = bool "\n" "\r\n" ("\r\n" `T.isInfixOf` t)

-- | The text with every line ending the given one.
withEol :: Text -> Text -> Text
withEol eol = T.replace "\n" eol . T.replace "\r\n" "\n"

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
