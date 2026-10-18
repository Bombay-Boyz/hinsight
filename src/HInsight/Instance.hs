-- |
-- Module      : HInsight.Instance
-- Description : A class constraint GHC could not solve, with the instances GHC lists for it.
--
-- An 'UnresolvedInstance' states the constraint GHC reported, where it arose,
-- and the instances GHC lists as potentially matching it. The list is GHC's;
-- nothing here decides which instance was meant or why none applied. An empty
-- list means GHC listed none, not that none exist.
module HInsight.Instance
  ( InstanceText,
    mkInstanceText,
    unInstanceText,
    UnresolvedInstance (..),
    shownInstances,
    shownCandidates,
    renderUnresolvedInstance,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import HInsight.Context (Context, renderContext)
import HInsight.Error (DomainError (..))
import HInsight.Source (Span, renderPosition, spanStart)
import HInsight.TypeText (TypeText, unTypeText)

-- | An instance as GHC printed it, for example @instance Show Int@ with the
-- module it comes from. Invariants: not blank, and on a single line with
-- single spaces between words.
newtype InstanceText = UnsafeInstanceText Text
  deriving stock (Eq, Ord, Show)

-- | Build an 'InstanceText'. Any run of whitespace becomes one space; the
-- result must contain at least one non-space character.
mkInstanceText :: Text -> Either DomainError InstanceText
mkInstanceText t
  | T.null normalised = Left (BlankText "instance")
  | otherwise = Right (UnsafeInstanceText normalised)
  where
    normalised :: Text
    normalised = T.unwords (T.words t)

-- | The text inside an 'InstanceText'.
unInstanceText :: InstanceText -> Text
unInstanceText (UnsafeInstanceText t) = t

-- | One class constraint GHC could not solve.
data UnresolvedInstance = UnresolvedInstance
  { unresolvedSpan :: !Span,
    -- | The constraint as GHC printed it, for example @Show T@.
    unresolvedConstraint :: !TypeText,
    -- | The instances GHC lists as potentially matching, in GHC's order. All
    -- of them are kept; 'shownCandidates' says how many a report shows.
    unresolvedPotential :: ![InstanceText],
    unresolvedContext :: !Context
  }
  deriving stock (Eq, Show)

-- | How many potentially matching instances a report shows before saying how
-- many were left out. A presentation limit only: the data keeps all of them.
shownInstances :: Int
shownInstances = 5

-- | The instances a report shows, and how many it leaves out.
shownCandidates :: UnresolvedInstance -> ([InstanceText], Int)
shownCandidates u = (shown, length omitted)
  where
    shown :: [InstanceText]
    omitted :: [InstanceText]
    (shown, omitted) = splitAt shownInstances (unresolvedPotential u)

-- | Render as plain lines, starting with the position: the constraint, the
-- instances GHC lists, and the context line when there is one.
renderUnresolvedInstance :: UnresolvedInstance -> Text
renderUnresolvedInstance u =
  T.unlines
    ( [ "unresolved constraint at " <> renderPosition (spanStart (unresolvedSpan u)),
        "  GHC could not solve: " <> unTypeText (unresolvedConstraint u)
      ]
        <> candidateLines u
        <> renderContext (unresolvedContext u)
    )

candidateLines :: UnresolvedInstance -> [Text]
candidateLines u = case shownCandidates u of
  ([], _) -> ["  potentially matching instances: none listed by GHC"]
  (shown, omitted) ->
    ("  potentially matching instances (" <> count <> "):")
      : map (("    " <>) . unInstanceText) shown
        <> ["    and " <> T.pack (show omitted) <> " more" | omitted > 0]
  where
    count :: Text
    count = T.pack (show (length (unresolvedPotential u)))
