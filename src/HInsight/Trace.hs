-- |
-- Module      : HInsight.Trace
-- Description : What GHC's typechecker trace says, as plain data.
--
-- With @-ddump-tc-trace -dppr-debug -ferror-spans@ GHC logs one message per
-- trace point, each starting with the span it was checking. This module reads
-- one such message at a time ('parseRecord') and keeps a bounded number of the
-- ones a caller wants ('capture'). It reads only the headings GHC 9.10.3
-- prints for unification; every other heading is kept as 'Unmodelled', and a
-- heading it knows with a shape it does not recognise is kept as 'Malformed'.
-- Nothing is inferred: the raw message is carried beside everything read from
-- it.
--
-- The layout of a message depends on the width GHC renders to, so the reader
-- joins all lines of a message into one before looking at it.
module HInsight.Trace
  ( -- * Reading a record
    Located (..),
    TraceRecord (..),
    Step (..),
    parseRecord,
    recordText,
    locatedIn,
    parseLocated,

    -- * A view of a step
    stepKindLevel,
    renderStep,
    cleanedSummaries,

    -- * Keeping a bounded number of records
    RecordLimit,
    mkRecordLimit,
    defaultRecordLimit,
    Capture,
    emptyCapture,
    capture,
    Trace (..),
    finishCapture,
    malformedCount,
  )
where

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import HInsight.Error (DomainError (..), TraceError (..))
import HInsight.Source
  ( SourceFile,
    Span,
    mkColumn,
    mkLine,
    mkPosition,
    mkSourceFile,
    mkSpan,
    sourceFilePath,
    spanContains,
  )
import HInsight.Trace.Clean (cleanBlock)
import HInsight.TypeText (TypeText, mkTypeText, unTypeText)
import System.FilePath (normalise)

-- | Where a record says it was working: a file, as GHC printed the path, and a
-- span. The end is exclusive, like every 'Span'. GHC prints a span of width 0
-- or 1 as a single column, so such a span reads back with width 1.
data Located = Located
  { locatedFile :: !SourceFile,
    locatedSpan :: !Span
  }
  deriving stock (Eq, Show)

-- | One message of the trace.
data TraceRecord = TraceRecord
  { -- | Absent when the message does not start with a span.
    recordLocation :: !(Maybe Located),
    recordStep :: !Step,
    -- | The message exactly as GHC logged it.
    recordRaw :: !Text
  }
  deriving stock (Eq, Show)

-- | What a record says. Closed by design (Standard 0.2); the types are as GHC
-- printed them under @-dppr-debug@, and 'cleanedSummaries' gives a readable
-- view.
data Step
  = -- | @u_tys@: GHC unifies two types, at a level of the type checker.
    Unify !Int !TypeText !TypeText
  | -- | @u_tys yields no coercion@: a unification finished without evidence.
    UnifySucceeded
  | -- | @u_tys yields coercion@: a unification finished with this evidence.
    UnifyCoerced !Text
  | -- | @utype_defer@: GHC gave up on an equality and deferred it. The role
    -- (such as @nominal@) and the rest of what GHC printed, which holds the two
    -- types and the context.
    Deferral !Text !Text
  | -- | @utype_defer2@: the evidence GHC deferred with.
    DeferralCoercion !Text
  | -- | @found filled tyvar@: a type variable already had this type.
    TyVarFilled !TypeText !TypeText
  | -- | @uUnfilledVar2@: whether GHC could fill a type variable, and what it
    -- printed about it.
    TyVarUnify !Bool !Text
  | -- | @tc_sub_type@: the actual type, then the expected type.
    SubTypeCheck !TypeText !TypeText
  | -- | @tcInstFun@: a function and its type, as GHC printed them.
    FunctionInstantiated !Text !TypeText
  | -- | @tcInstFun:ret@: the result type after instantiating a function.
    FunctionResult !TypeText
  | -- | A heading this module does not interpret.
    Unmodelled !Text
  | -- | A heading this module interprets, with a shape it did not recognise.
    Malformed !Text
  deriving stock (Eq, Show)

-- | Read one message. Total: every text gives a record, and 'recordRaw' is the
-- text given.
parseRecord :: Text -> TraceRecord
parseRecord raw = TraceRecord location (parseStep flat) raw
  where
    location :: Maybe Located
    body :: Text
    (location, body) = splitLocation raw
    flat :: Text
    flat = T.unwords (T.words body)

-- | What a record says after its location, with every run of whitespace as
-- one space. This is the text the steps are read from.
recordText :: TraceRecord -> Text
recordText = T.unwords . T.words . snd . splitLocation . recordRaw

-- | Whether a record says it was working in this file, inside one of these
-- spans. The paths are compared after normalising, so @./A.hs@ and @A.hs@ are
-- the same file. A record with no location is never inside.
locatedIn :: FilePath -> [Span] -> TraceRecord -> Bool
locatedIn file spans r = case recordLocation r of
  Just l ->
    normalise (sourceFilePath (locatedFile l)) == normalise file
      && any (`spanContains` locatedSpan l) spans
  Nothing -> False

-- A message starts with @file:span::@. Anything else has no location.
splitLocation :: Text -> (Maybe Located, Text)
splitLocation t = case T.breakOn "::" (T.stripStart t) of
  (prefix, rest)
    | not (T.null rest),
      Just located <- parseLocated prefix ->
        (Just located, T.drop 2 rest)
  _ -> (Nothing, t)

parseLocated :: Text -> Maybe Located
parseLocated prefix
  | T.any (== '\n') prefix = Nothing
  | otherwise = case reverse (T.splitOn ":" prefix) of
      lastSegment : rest
        | Just (l1, c1, l2, c2) <- multiLine lastSegment -> build (joinPath rest) l1 c1 l2 c2
      columns : line : rest
        | Just l <- readInt line,
          Just (c1, c2) <- columnRange columns ->
            build (joinPath rest) l c1 l c2
      _ -> Nothing
  where
    joinPath :: [Text] -> Text
    joinPath = T.intercalate ":" . reverse

-- @(l1,c1)-(l2,c2)@, printed with an inclusive end; the result is exclusive.
multiLine :: Text -> Maybe (Int, Int, Int, Int)
multiLine s = do
  a <- T.stripPrefix "(" s
  let (start, b) = T.breakOn ")-(" a
  end <- T.stripPrefix ")-(" b >>= T.stripSuffix ")"
  (l1, c1) <- pair start
  (l2, c2) <- pair end
  -- GHC prints an end column of 0 as 0; any other is printed one low.
  Just (l1, c1, l2, if c2 == 0 then 0 else c2 + 1)
  where
    pair :: Text -> Maybe (Int, Int)
    pair x = case T.splitOn "," x of
      [l, c] -> (,) <$> readInt l <*> readInt c
      _ -> Nothing

-- @c@ or @c1-c2@, printed with an inclusive end; the result is exclusive.
columnRange :: Text -> Maybe (Int, Int)
columnRange t = case T.splitOn "-" t of
  [c] -> (\a -> (a, a + 1)) <$> readInt c
  [c1, c2] -> (\a b -> (a, b + 1)) <$> readInt c1 <*> readInt c2
  _ -> Nothing

build :: Text -> Int -> Int -> Int -> Int -> Maybe Located
build file l1 c1 l2 c2 = do
  path <- ok (mkSourceFile (T.unpack file))
  start <- mkPosition <$> ok (mkLine l1) <*> ok (mkColumn c1)
  end <- mkPosition <$> ok (mkLine l2) <*> ok (mkColumn c2)
  Located path <$> ok (mkSpan start end)
  where
    ok :: Either e a -> Maybe a
    ok = either (const Nothing) Just

readInt :: Text -> Maybe Int
readInt t = case TR.decimal t of
  Right (n, rest) | T.null rest && T.length t <= 9 -> Just n
  _ -> Nothing

-- | The headings read, each as a literal that starts the flattened message and
-- a reader for what follows it. A reader that gives 'Nothing' means the shape
-- was not recognised.
vocabulary :: [(Text, Text -> Maybe Step)]
vocabulary =
  [ ("u_tys yields no coercion", \p -> if T.null p then Just UnifySucceeded else Nothing),
    ("u_tys yields coercion:", nonEmpty UnifyCoerced),
    ("u_tys tclvl", unify),
    ("utype_defer2", nonEmpty DeferralCoercion),
    ("utype_defer", deferral),
    ("found filled tyvar", filled),
    ("uUnfilledVar2 not ok", Just . TyVarUnify False),
    ("uUnfilledVar2 ok", Just . TyVarUnify True),
    ("tc_sub_type (general case)", subType),
    ("tcInstFun:ret", fmap FunctionResult . typeText),
    ("tcInstFun tc_fun", instFun)
  ]
  where
    nonEmpty :: (Text -> Step) -> Text -> Maybe Step
    nonEmpty f p = if T.null p then Nothing else Just (f p)

    unify :: Text -> Maybe Step
    unify p = do
      let (levelText, rest) = T.breakOn " " p
      level <- readInt levelText
      let (left, afterLeft) = T.breakOn " ~nominal " rest
      right <- T.stripPrefix " ~nominal " afterLeft
      Unify level <$> typeText left <*> typeText right

    deferral :: Text -> Maybe Step
    deferral p = case T.words p of
      role : _ -> Just (Deferral role (T.strip (T.drop (T.length role) p)))
      [] -> Nothing

    filled :: Text -> Maybe Step
    filled p = do
      let (var, rest) = T.breakOn " :-> " p
      ty <- T.stripPrefix " :-> " rest
      TyVarFilled <$> typeText var <*> typeText ty

    subType :: Text -> Maybe Step
    subType p = do
      afterActual <- T.stripPrefix "ty_actual = " p
      let (actual, rest) = T.breakOn " ty_expected = " afterActual
      expected <- T.stripPrefix " ty_expected = " rest
      SubTypeCheck <$> typeText actual <*> typeText expected

    instFun :: Text -> Maybe Step
    instFun p = do
      let (function, rest) = T.breakOn " fun_sigma " p
      afterSigma <- T.stripPrefix " fun_sigma " rest
      let sigma = fst (T.breakOn " fun_ctxt " afterSigma)
      if T.null (T.strip function) then Nothing else FunctionInstantiated (T.strip function) <$> typeText sigma

    typeText :: Text -> Maybe TypeText
    typeText = either (const Nothing) Just . mkTypeText

parseStep :: Text -> Step
parseStep flat = fromMaybe (Unmodelled heading) (firstJust (map try vocabulary))
  where
    heading :: Text
    heading = fst (T.breakOn " " flat)

    try :: (Text, Text -> Maybe Step) -> Maybe Step
    try (literal, reader) = do
      rest <- T.stripPrefix literal flat
      if atBoundary rest || ":" `T.isSuffixOf` literal
        then Just (fromMaybe (Malformed heading) (reader (T.strip rest)))
        else Nothing

    atBoundary :: Text -> Bool
    atBoundary rest = maybe True ((== ' ') . fst) (T.uncons rest)

    firstJust :: [Maybe a] -> Maybe a
    firstJust = foldr (<|>) Nothing

-- | Whether a unification is certainly between kinds, judged only by its
-- printed form: every word on both sides is one of GHC's own kind names. A
-- step that mixes in a type variable is not marked, even if it is about kinds.
-- Steps are marked for the reader and never hidden.
stepKindLevel :: Step -> Bool
stepKindLevel = \case
  Unify _ l r -> all (`S.member` kindWords) (kindTokens l ++ kindTokens r)
  _ -> False
  where
    kindTokens :: TypeText -> [Text]
    kindTokens = T.words . T.map (\c -> if c == '(' || c == ')' then ' ' else c) . unTypeText

kindWords :: S.Set Text
kindWords =
  S.fromList
    [ "GHC.Types.Type",
      "GHC.Types.RuntimeRep",
      "GHC.Types.LiftedRep",
      "GHC.Types.Constraint",
      "GHC.Types.Many",
      "GHC.Prim.TYPE",
      "*",
      "->"
    ]

-- | A one-line description of a step, using GHC's own type text.
renderStep :: Step -> Text
renderStep = \case
  Unify _ l r -> "unify " <> unTypeText l <> " with " <> unTypeText r
  UnifySucceeded -> "unification finished with no coercion"
  UnifyCoerced co -> "unification finished with coercion " <> co
  Deferral role detail -> "GHC deferred the " <> role <> " equality: " <> detail
  DeferralCoercion co -> "deferred with evidence " <> co
  TyVarFilled v t -> "variable " <> unTypeText v <> " was already " <> unTypeText t
  TyVarUnify True d -> "GHC could fill a variable: " <> d
  TyVarUnify False d -> "GHC could not fill a variable: " <> d
  SubTypeCheck a e -> "check actual " <> unTypeText a <> " against expected " <> unTypeText e
  FunctionInstantiated f s -> "instantiate " <> f <> " :: " <> unTypeText s
  FunctionResult t -> "instantiated result " <> unTypeText t
  Unmodelled h -> h <> " (not interpreted)"
  Malformed h -> h <> " (shape not recognised)"

-- | 'renderStep' for each record, cleaned together so that a module qualifier
-- is dropped only where no two names would collide. One entry per record.
cleanedSummaries :: [TraceRecord] -> [Text]
cleanedSummaries = cleanBlock . map (renderStep . recordStep)

-- | The most records a 'Capture' keeps. Invariant: at least 1.
newtype RecordLimit = UnsafeRecordLimit Int
  deriving stock (Eq, Show)

-- | Build a 'RecordLimit'.
mkRecordLimit :: Int -> Either DomainError RecordLimit
mkRecordLimit n
  | n >= 1 = Right (UnsafeRecordLimit n)
  | otherwise = Left (NonPositiveLimit "record limit" n)

-- | The limit used when the caller names none. Measured on GHC 9.10.3, a line
-- of a module produces about 110 records (121 lines gave 13,681 and 226 lines
-- gave 25,187), and the records of every top-level binding are kept, not only
-- those of the binding with the error. A kept record costs up to about 2 KB, so
-- this bounds the records at about 200 MB and suits a module of about 900
-- lines. A larger one gives 'TraceLimitExceeded' rather than a partial trace.
defaultRecordLimit :: RecordLimit
defaultRecordLimit = UnsafeRecordLimit 100000

-- | A trace being collected. It keeps no more than its limit, and once the
-- limit is passed it keeps nothing, so memory stays bounded however long the
-- trace is.
data Capture = Capture
  { capLimit :: !Int,
    capSeen :: !Int,
    capKept :: !Int,
    capRecords :: [TraceRecord],
    capOverflow :: !Bool
  }

-- | An empty 'Capture'.
emptyCapture :: RecordLimit -> Capture
emptyCapture (UnsafeRecordLimit n) = Capture n 0 0 [] False

-- | Offer one message. It is read, and kept only when the predicate accepts
-- the record.
capture :: (TraceRecord -> Bool) -> Text -> Capture -> Capture
capture wanted message c
  | not (wanted record) = seen
  | capOverflow c = seen
  | capKept c >= capLimit c = seen {capOverflow = True, capRecords = []}
  | otherwise = seen {capKept = capKept c + 1, capRecords = record : capRecords c}
  where
    record :: TraceRecord
    record = parseRecord message
    seen :: Capture
    seen = c {capSeen = capSeen c + 1}

-- | The records kept, in the order GHC logged them.
data Trace = Trace
  { traceRecords :: [TraceRecord],
    -- | How many messages were offered, kept or not.
    traceSeen :: !Int
  }
  deriving stock (Eq, Show)

-- | The result of a 'Capture'. Past the limit the answer is an error, never a
-- part of the trace.
finishCapture :: Capture -> Either TraceError Trace
finishCapture c
  | capOverflow c = Left (TraceLimitExceeded (capLimit c))
  | otherwise = Right (Trace (reverse (capRecords c)) (capSeen c))

-- | How many kept records have a heading that is read but a shape that is not
-- recognised.
malformedCount :: Trace -> Int
malformedCount = length . mapMaybe malformed . traceRecords
  where
    malformed :: TraceRecord -> Maybe Text
    malformed r = case recordStep r of
      Malformed h -> Just h
      _ -> Nothing
