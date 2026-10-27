-- |
-- Module      : HInsight.Splice
-- Description : What GHC says each Template Haskell splice of a file became.
--
-- With @-ddump-splices@ GHC logs one message per splice that it ran: where the
-- splice is, what kind it is, the code of the splice and the code it produced.
-- This module reads those messages and reports them; it runs nothing and works
-- out nothing.
--
-- GHC lays a message out to the width it renders to, so the same splice prints
-- on one line or on several. The reader joins the lines of a message before it
-- reads it, and the readings agree at every width (the test compares widths 40,
-- 100 and 200). Only the lines of a declaration splice's result are kept apart,
-- because there each line starts a declaration; the lines that start at the
-- least indentation begin a declaration and deeper lines continue the one
-- before.
--
-- What is carried, and what is not:
--
-- * The kind is the word GHC prints (@expression@, @declarations@, @type@,
--   @pattern@); a word not in that list is kept as it is.
-- * A typed splice and a quasi-quote are printed as an expression, so they
--   are reported as one. GHC does not say they are anything else.
-- * A splice inside the quote of another splice is not a record of its own.
--   GHC notes it on the outer splice as @pending(rn) [...]@; those notes are
--   carried as GHC printed them and not taken apart.
-- * GHC dumps splices in the order it ran them, which is not source order (a
--   declaration group is run before the expressions in it). 'buildSplices'
--   lists them by position.
module HInsight.Splice
  ( SpliceKind (..),
    Splice,
    spliceSpan,
    spliceKind,
    spliceSource,
    splicePending,
    spliceResult,
    spliceRaw,
    SpliceRead (..),
    readSplice,
    splicesIn,
    Splices (..),
    emptySplices,
    buildSplices,
    renderSplice,
    renderSplices,
  )
where

import Data.Char (isAlpha, isSpace)
import Data.List (sortOn)
import Data.Text (Text)
import Data.Text qualified as T
import HInsight.Source (Span, renderPosition, sourceFilePath, spanStart)
import HInsight.Trace (Located (..), TraceRecord (..), parseLocated)
import System.FilePath (normalise)

-- | The kind of splice, as GHC names it in the dump.
data SpliceKind
  = ExpressionSplice
  | DeclarationSplice
  | TypeSplice
  | PatternSplice
  | -- | A word this module does not know, as GHC printed it.
    OtherSplice !Text
  deriving stock (Eq, Show)

-- | One splice GHC ran.
data Splice = Splice
  { spliceLocated :: !Located,
    spliceKind :: !SpliceKind,
    -- | The code of the splice on one line, without the notes of nested
    -- splices.
    spliceSource :: !Text,
    -- | GHC's notes of the splices nested in this one, each as printed
    -- (@pending(rn) [...]@) on one line.
    splicePending :: ![Text],
    -- | The code that GHC says the splice produced. A declaration splice has
    -- one entry per declaration line GHC printed; any other has at most one.
    spliceResult :: ![Text],
    -- | The message as GHC logged it.
    spliceRaw :: !Text
  }
  deriving stock (Eq, Show)

-- | Where the splice is.
spliceSpan :: Splice -> Span
spliceSpan = locatedSpan . spliceLocated

-- | What one message came to.
data SpliceRead
  = -- | A splice, read.
    SpliceRead !Splice
  | -- | A message that says @Splicing@ at a location but has a shape this
    -- module does not recognise. It is counted, not dropped.
    SpliceUnread !Located !Text
  | -- | Not a splice message.
    NotASplice
  deriving stock (Eq, Show)

-- | Read one message. Total.
readSplice :: Text -> SpliceRead
readSplice raw = case T.breakOn marker (T.stripStart raw) of
  (prefix, rest)
    | not (T.null rest),
      Just location <- parseLocated prefix ->
        fromBody location (T.drop (T.length marker) rest)
  _ -> NotASplice
  where
    marker :: Text
    marker = ": Splicing "
    fromBody :: Located -> Text -> SpliceRead
    fromBody location body = case T.break isSpace (T.stripStart body) of
      (word, afterWord)
        | not (T.null word),
          (source, arrowAndResult) <- T.breakOn arrow afterWord,
          not (T.null arrowAndResult) ->
            let kind = kindOf word
                (plain, pending) = extractPending (flatten source)
                result = resultItems kind (T.drop (T.length arrow) arrowAndResult)
             in SpliceRead (Splice location kind plain pending result raw)
      _ -> SpliceUnread location raw

arrow :: Text
arrow = "======>"

kindOf :: Text -> SpliceKind
kindOf = \case
  "expression" -> ExpressionSplice
  "declarations" -> DeclarationSplice
  "type" -> TypeSplice
  "pattern" -> PatternSplice
  other -> OtherSplice other

flatten :: Text -> Text
flatten = T.unwords . T.words

-- Take the notes @pending(name) [...]@ out of a flattened source. A note whose
-- bracket is never closed is left in the source.
extractPending :: Text -> (Text, [Text])
extractPending t = case T.breakOn "pending(" t of
  (before, rest)
    | T.null rest -> (T.strip before, [])
    | Just (note, after) <- pendingNote rest ->
        let (others, notes) = extractPending after
         in (T.unwords (filter (not . T.null) [T.strip before, others]), note : notes)
    | otherwise -> (t, [])

-- @pending(name) [ ... ]@ at the start of the text, with the rest after it.
pendingNote :: Text -> Maybe (Text, Text)
pendingNote t = do
  afterOpen <- T.stripPrefix "pending(" t
  let (name, afterName) = T.span isAlpha afterOpen
  afterBracket <- T.stripPrefix ") [" afterName
  (inside, after) <- closing (T.unpack afterBracket) (1 :: Int) []
  Just ("pending(" <> name <> ") [" <> T.pack (reverse inside) <> "]", T.pack after)
  where
    closing :: String -> Int -> String -> Maybe (String, String)
    closing [] _ _ = Nothing
    closing (c : cs) depth acc
      | c == '[' = closing cs (depth + 1) (c : acc)
      | c == ']' = if depth == 1 then Just (acc, cs) else closing cs (depth - 1) (c : acc)
      | otherwise = closing cs depth (c : acc)

resultItems :: SpliceKind -> Text -> [Text]
resultItems kind afterArrow = case kind of
  DeclarationSplice -> declarations (filter (not . T.null . T.strip) (T.lines afterArrow))
  _ -> [flat | let flat = flatten afterArrow, not (T.null flat)]

-- A line at the least indentation starts a declaration; deeper lines continue
-- it.
declarations :: [Text] -> [Text]
declarations ls = case ls of
  [] -> []
  first : others -> group (foldr (min . indent) (indent first) others) ls
  where
    group :: Int -> [Text] -> [Text]
    group _ [] = []
    group base (l : rest) =
      let (continued, more) = span ((> base) . indent) rest
       in flatten (T.unwords (l : continued)) : group base more
    indent :: Text -> Int
    indent = T.length . T.takeWhile (== ' ')

-- | Whether a captured message is a splice message of this file. The paths are
-- compared after normalising.
splicesIn :: FilePath -> TraceRecord -> Bool
splicesIn file r = case readSplice (recordRaw r) of
  SpliceRead s -> sameFile (spliceLocated s)
  SpliceUnread location _ -> sameFile location
  NotASplice -> False
  where
    sameFile :: Located -> Bool
    sameFile l = normalise (sourceFilePath (locatedFile l)) == normalise file

-- | The splices of a file.
data Splices = Splices
  { -- | The splices read, by position.
    splicesFound :: ![Splice],
    -- | Splice messages that were not read.
    splicesUnread :: !Int,
    -- | Errors that stopped GHC before it could run any splice, such as a
    -- parse error or an import it could not find.
    splicesBlocked :: !Int
  }
  deriving stock (Eq, Show)

-- | No splices, and nothing wrong.
emptySplices :: Splices
emptySplices = Splices [] 0 0

-- | The splices in the messages GHC logged, in the order it logged them.
buildSplices :: [Text] -> Splices
buildSplices messages = Splices (sortOn (spanStart . spliceSpan) found) unread 0
  where
    reads' :: [SpliceRead]
    reads' = map readSplice messages
    found :: [Splice]
    found = [s | SpliceRead s <- reads']
    unread :: Int
    unread = length [() | SpliceUnread _ _ <- reads']

-- | One splice as plain lines.
renderSplice :: Splice -> Text
renderSplice s =
  T.unlines
    ( (renderPosition (spanStart (spliceSpan s)) <> "  " <> kindName (spliceKind s) <> " splice")
        : ("    source: " <> spliceSource s)
        : map ("    GHC noted nested splices: " <>) (splicePending s)
          <> result
    )
  where
    result :: [Text]
    result = case spliceResult s of
      [] -> ["    produced: nothing"]
      [one] -> ["    produced: " <> one]
      many -> "    produced:" : map ("      " <>) many
    kindName :: SpliceKind -> Text
    kindName = \case
      ExpressionSplice -> "expression"
      DeclarationSplice -> "declaration"
      TypeSplice -> "type"
      PatternSplice -> "pattern"
      OtherSplice word -> word

-- | Every splice, then what could not be said.
renderSplices :: Splices -> Text
renderSplices ss = T.concat (map renderSplice (splicesFound ss)) <> footer
  where
    footer :: Text
    footer = T.concat (blocked <> unread <> none)
    blocked :: [Text]
    blocked =
      [ T.pack (show (splicesBlocked ss)) <> " error(s) stopped GHC before it could run any splice\n"
      | splicesBlocked ss > 0
      ]
    unread :: [Text]
    unread =
      [ T.pack (show (splicesUnread ss)) <> " splice message(s) could not be read\n"
      | splicesUnread ss > 0
      ]
    none :: [Text]
    none = ["no splices: GHC ran none in this file\n" | null (splicesFound ss), splicesBlocked ss == 0, splicesUnread ss == 0]
