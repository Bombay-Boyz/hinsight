-- |
-- Module      : HInsight.Core
-- Description : What GHC says about the Core it made for a file.
--
-- With @-ddump-simpl@, @-ddump-spec@ and @-ddump-rule-firings@ GHC logs its
-- own account of what it did to a module: the Core before and after the
-- simplifier, and every rewrite rule it applied. This module reads those
-- messages and reports them; it works out nothing.
--
-- * A section is one dump, headed @==================== Name ====================@.
--   Its body is carried as GHC printed it. GHC lays Core out to the width it
--   renders to, so the body depends on the width and is not parsed.
-- * A rule firing is one line, @Rule fired: name (module)@. The module is where
--   the rule is defined; GHC does not say which module of the compile fired it,
--   so the caller compiles only the file asked about.
-- * With no optimisation GHC fires no rules and prints no specialisation
--   section, and the result says exactly that.
module HInsight.Core
  ( Optimisation (..),
    CoreDetail (..),
    CoreRequest (..),
    coreOnly,
    SectionKind (..),
    sectionKind,
    CoreSection (..),
    RuleFiring (..),
    CoreRead (..),
    readCore,
    isCoreMessage,
    CoreDump (..),
    emptyCore,
    buildCore,
    renderCore,
  )
where

import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import HInsight.Trace (TraceRecord (..))

-- | How much GHC optimises the file when it makes its Core.
data Optimisation
  = -- | @-O0@: the Core as desugared and tidied; no rule fires.
    Unoptimised
  | -- | @-O@: the simplifier runs, so rules fire and specialisations appear.
    Optimised
  deriving stock (Eq, Show)

-- | How much of the compiler's output to ask for.
data CoreDetail
  = -- | Core, specialisation and rule firings.
    CoreOnly
  | -- | The same, and the STG GHC makes. This compiles the file and its project imports to object code.
    CoreAndStg
  deriving stock (Eq, Show)

-- | What a Core question asks for.
data CoreRequest = CoreRequest
  { requestOptimisation :: !Optimisation,
    requestDetail :: !CoreDetail
  }
  deriving stock (Eq, Show)

-- | A request for Core without STG.
coreOnly :: Optimisation -> CoreRequest
coreOnly o = CoreRequest o CoreOnly

-- | One dump GHC logged.
data CoreSection = CoreSection
  { -- | The name in the header, as printed (@Tidy Core@, @Specialise@).
    sectionName :: !Text,
    -- | What follows the header, as printed.
    sectionBody :: !Text
  }
  deriving stock (Eq, Show)

-- | One rule GHC applied.
data RuleFiring = RuleFiring
  { -- | The rule's name, as printed (@fold/build@, @Class op +@).
    ruleName :: !Text,
    -- | The module that defines the rule, or @BUILTIN@.
    ruleModule :: !Text
  }
  deriving stock (Eq, Ord, Show)

-- | Which dump a section is, by the name in its header.
data SectionKind = TidyCore | Specialisation | FinalStg | OtherSection
  deriving stock (Eq, Show)

-- | The kind of a section. A name not listed is 'OtherSection'; nothing is dropped.
sectionKind :: CoreSection -> SectionKind
sectionKind s
  | name == "Tidy Core" = TidyCore
  | "Specialise" `T.isPrefixOf` name = Specialisation
  | "Final STG" `T.isPrefixOf` name = FinalStg
  | otherwise = OtherSection
  where
    name :: Text
    name = sectionName s

-- | What one message came to.
data CoreRead
  = ReadSection !CoreSection
  | ReadRules ![RuleFiring]
  | -- | A message that looks like a dump or a rule firing but has a shape this
    -- module does not recognise. It is counted, not dropped.
    UnreadCore !Text
  | NotCore
  deriving stock (Eq, Show)

-- | Read one message. Total.
readCore :: Text -> CoreRead
readCore raw
  | "Rule fired: " `T.isPrefixOf` stripped = maybe (UnreadCore raw) ReadRules (traverse firing (T.lines stripped))
  | "====" `T.isPrefixOf` stripped = maybe (UnreadCore raw) ReadSection (section stripped)
  | otherwise = NotCore
  where
    stripped :: Text
    stripped = T.dropWhile (== '\n') (T.stripStart raw)

-- A line @Rule fired: name (module)@. The module is the last parenthesised
-- word, because a rule's name may itself hold parentheses.
firing :: Text -> Maybe RuleFiring
firing line = do
  rest <- T.stripPrefix "Rule fired: " (T.stripEnd line)
  body <- T.stripSuffix ")" rest
  let (nameAndSpace, moduleText) = T.breakOnEnd " (" body
  name <- T.stripSuffix " (" nameAndSpace
  if T.null name || T.null moduleText || T.any (== ' ') moduleText
    then Nothing
    else Just (RuleFiring name moduleText)

-- A header line of equals signs around a name, then the body.
section :: Text -> Maybe CoreSection
section t = case T.lines t of
  header : body -> do
    inner <- T.stripPrefix " " (T.dropWhile (== '=') header)
    name <- T.stripSuffix " " (T.dropWhileEnd (== '=') inner)
    if T.null (T.strip name)
      then Nothing
      else Just (CoreSection (T.strip name) (T.intercalate "\n" (trimBlank body)))
  [] -> Nothing
  where
    trimBlank :: [Text] -> [Text]
    trimBlank = reverse . dropWhile (T.null . T.strip) . reverse . dropWhile (T.null . T.strip)

-- | Whether a captured message is one the Core mode wants. Messages about
-- anything else (progress lines, say) are not counted.
isCoreMessage :: TraceRecord -> Bool
isCoreMessage r = case readCore (recordRaw r) of
  NotCore -> False
  _ -> True

-- | What GHC said about the Core of a file.
data CoreDump = CoreDump
  { coreRequest :: !CoreRequest,
    -- | The dumps, in the order GHC logged them.
    coreSections :: ![CoreSection],
    -- | The rules applied, in the order GHC logged them.
    coreRules :: ![RuleFiring],
    -- | Messages that were not read.
    coreUnread :: !Int,
    -- | Errors that stopped GHC before it made any Core, such as a type error
    -- or a parse error: Core is made only for a file that typechecks.
    coreBlocked :: !Int
  }
  deriving stock (Eq, Show)

-- | Nothing logged and nothing wrong.
emptyCore :: CoreRequest -> CoreDump
emptyCore r = CoreDump r [] [] 0 0

-- | The dump in the messages GHC logged, in the order it logged them.
buildCore :: CoreRequest -> [Text] -> CoreDump
buildCore r messages = CoreDump r [s | ReadSection s <- reads'] (concat [rs | ReadRules rs <- reads']) (length [() | UnreadCore _ <- reads']) 0
  where
    reads' :: [CoreRead]
    reads' = map readCore messages

-- | The dump as plain text: what was asked, the rules by how often each fired,
-- then each section as GHC printed it.
renderCore :: CoreDump -> Text
renderCore d = T.unlines (heading : blocked <> unread <> noStg <> rules <> concatMap sectionLines (coreSections d))
  where
    heading :: Text
    heading =
      "what GHC logged for the Core of this file ("
        <> modeName (requestOptimisation (coreRequest d))
        <> (if wantsStg then ", with STG" else "")
        <> "): "
        <> count (length (coreSections d)) "section"
        <> ", "
        <> count (length (coreRules d)) "rule firing"
    wantsStg :: Bool
    wantsStg = requestDetail (coreRequest d) == CoreAndStg
    noStg :: [Text]
    noStg = ["  no STG: GHC logged none" | wantsStg, coreBlocked d == 0, FinalStg `notElem` map sectionKind (coreSections d)]
    modeName :: Optimisation -> Text
    modeName = \case
      Optimised -> "optimised"
      Unoptimised -> "unoptimised"
    count :: Int -> Text -> Text
    count n word = T.pack (show n) <> " " <> word <> (if n == 1 then "" else "s")
    blocked :: [Text]
    blocked = ["  " <> T.pack (show (coreBlocked d)) <> " error(s) stopped GHC before it made any Core" | coreBlocked d > 0]
    unread :: [Text]
    unread = ["  " <> T.pack (show (coreUnread d)) <> " message(s) could not be read" | coreUnread d > 0]
    rules :: [Text]
    rules = map ruleLine (sortOn (\(r, n) -> (Down n, r)) (M.toList (M.fromListWith (+) [(r, 1 :: Int) | r <- coreRules d])))
    ruleLine :: (RuleFiring, Int) -> Text
    ruleLine (r, n) = "  " <> T.pack (show n) <> " x " <> ruleName r <> " (" <> ruleModule r <> ")"
    sectionLines :: CoreSection -> [Text]
    sectionLines s = "" : ("==================== " <> sectionName s <> " ====================") : T.lines (sectionBody s)
