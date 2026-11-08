-- |
-- Module      : HInsight.Refactor.Move
-- Description : The pure part of moving a top-level definition to another module.
module HInsight.Refactor.Move
  ( Anchor (..),
    ExportClose (..),
    MoveFacts (..),
    planMove,
  )
where

import Control.Monad (unless)
import Data.Bifunctor (first)
import Data.Char (isAlpha, isSpace)
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
    moveDefinition :: !Span,
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

-- | The edits that take the definition out of the source module and put it,
-- with the source's imports minus the ones listed as dropped, in the target.
planMove :: [Span] -> Text -> Text -> MoveFacts -> Either InsightError EditSet
planMove dropped fromSrc toSrc facts = do
  let definition = moveDefinition facts
      defLine = unLine (positionLine (spanStart definition))
  unless (unColumn (positionColumn (spanStart definition)) == 1) (refuse NotATopLevelDefinition)
  let firstLine = haddockStart fromLines defLine
  from <- domain (position firstLine 1)
  upTo <- removalEnd
  moved <- refuse' (slice fromSrc =<< domain' (mkSpan' from (spanEnd definition)))
  removal <- edit (moveFrom facts) from upTo ""
  eof <- domain (endOfText toSrc)
  added <- edit (moveTo facts) eof eof ((if "\n" `T.isSuffixOf` toSrc then "\n" else "\n\n") <> moved <> "\n")
  copied <- copiedImports
  imports <- anchored (moveTo facts) (moveToAnchor facts) copied
  back <- anchored (moveFrom facts) (moveFromAnchor facts) [importLine | moveKeepsImport facts]
  exports <- exportEdit
  domain (mkEditSet (removal : added : imports <> back <> exports))
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
      | endLine >= length fromLines = Right (spanEnd (moveDefinition facts))
      | otherwise = domain (position next 1)
      where
        endLine :: Int
        endLine = unLine (positionLine (spanEnd (moveDefinition facts)))
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
    copiedImports :: Either InsightError [Text]
    copiedImports = do
      wanted <- traverse (refuse' . slice fromSrc) (filter (`notElem` dropped) (moveFromImports facts))
      present <- traverse (refuse' . slice toSrc) (moveToImports facts)
      pure [t | t <- wanted, normalised t `notElem` map normalised present]
    normalised :: Text -> Text
    normalised = T.unwords . T.words
    anchored :: SourceFile -> Anchor -> [Text] -> Either InsightError [Edit]
    anchored _ _ [] = Right []
    anchored file anchor texts = case anchor of
      AfterImport p -> pure <$> edit file p p ("\n" <> T.intercalate "\n" texts)
      AfterModuleHeader p -> pure <$> edit file p p ("\n\n" <> T.intercalate "\n" texts)
    exportEdit :: Either InsightError [Edit]
    exportEdit = case moveToExports facts of
      Just (ExportClose (p, nonEmpty)) | moveKeepsImport facts -> pure <$> edit (moveTo facts) p p ((if nonEmpty then ", " else "") <> nameText)
      _ -> Right []

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
