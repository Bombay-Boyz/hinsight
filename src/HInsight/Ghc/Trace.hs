-- |
-- Module      : HInsight.Ghc.Trace
-- Description : Switch GHC's dumps on and collect what they log.
--
-- The trace is GHC's own output: with @-ddump-tc-trace -dppr-debug
-- -ferror-spans@ it logs one message per trace point, each starting with the
-- span being checked. A log hook receives each message as it is logged, so the
-- messages need no splitting, and keeps the ones a caller wants in a bounded
-- 'Capture' ("HInsight.Trace"). Nothing here reads a message; that is the pure
-- side's job.
module HInsight.Ghc.Trace
  ( traceFlags,
    coreBuildFlags,
    coreDumpFlags,
    stgDumpFlags,
    spliceFlags,
    RawMissed (..),
    missedHook,
    enableFlags,
    captureHook,
    errorSpans,
  )
where

import Control.Monad (void)
import Data.Foldable (toList)
import Data.IORef (IORef, modifyIORef')
import Data.Text qualified as T
import GHC (Ghc, getLogger, getSessionDynFlags, parseDynamicFlags, setSessionDynFlags)
import GHC.Data.FastString (unpackFS)
import GHC.Driver.Errors.Types (GhcMessage)
import GHC.Driver.Flags (WarningFlag (..))
import GHC.Types.Error (DiagnosticReason (..), MessageClass (..), MsgEnvelope (..), ResolvedDiagnosticReason (..))
import GHC.Types.SrcLoc (SrcSpan (..), UnhelpfulSpanReason (..), noLoc, srcSpanFile, unLoc)
import GHC.Utils.Logger (LogAction, LogFlags (..))
import GHC.Utils.Outputable (renderWithContext)
import HInsight.Core (MissedKind (..), Optimisation (..))
import HInsight.Error (EnvironmentError (..), InsightError)
import HInsight.Ghc.Convert (convertSpan)
import HInsight.Source (Span)
import HInsight.Trace (Capture, TraceRecord, capture)

-- | The flags of the typechecker trace.
traceFlags :: [String]
traceFlags = ["-ddump-tc-trace", "-dppr-debug", "-ferror-spans"]

-- | The flags of the splice dump.
spliceFlags :: [String]
spliceFlags = ["-ddump-splices", "-ferror-spans"]

-- | The flags of a Core question with no dump on, and a folder for anything
-- GHC writes while it makes Core; the folder is the caller's to remove. They
-- are switched on before the dependencies of the file load, because a module
-- loaded without them is not optimised against later.
coreBuildFlags :: Optimisation -> FilePath -> [String]
coreBuildFlags optimisation folder = optimisationFlag optimisation : missedFlags <> folderFlags folder

-- | The dumps of Core, specialisation and rule firings, switched on once the
-- dependencies are loaded.
coreDumpFlags :: [String]
coreDumpFlags = coreDumps

-- | The dumps to switch on once the dependencies are compiled.
stgDumpFlags :: [String]
stgDumpFlags = coreDumps <> ["-ddump-stg-final"]

optimisationFlag :: Optimisation -> String
optimisationFlag = \case
  Optimised -> "-O"
  Unoptimised -> "-O0"

missedFlags :: [String]
missedFlags = ["-Wmissed-specialisations", "-Wall-missed-specialisations"]

coreDumps :: [String]
coreDumps = ["-ddump-simpl", "-ddump-spec", "-ddump-rule-firings"]

folderFlags :: FilePath -> [String]
folderFlags folder = ["-hidir", folder, "-odir", folder, "-stubdir", folder, "-dumpdir", folder <> "/"]

-- | Switch these flags on for the rest of the session. A module is
-- typechecked with the flags stored in its summary, so the caller builds the
-- module graph again after this.
enableFlags :: [String] -> Ghc (Either EnvironmentError ())
enableFlags wanted = do
  logger <- getLogger
  current <- getSessionDynFlags
  (flags, leftovers, _warnings) <- parseDynamicFlags logger current (map noLoc wanted)
  case leftovers of
    [] -> Right <$> void (setSessionDynFlags flags)
    unknown -> pure (Left (UnrecognisedArguments (map (T.pack . unLoc) unknown)))

-- | A log hook that gives each message that is not a diagnostic to the
-- 'Capture', which keeps it if the predicate accepts the record read from it,
-- and prints nothing. Diagnostics go on to the hook that was there before.
captureHook :: IORef Capture -> (TraceRecord -> Bool) -> LogAction -> LogAction
captureHook ref wanted original flags messageClass location doc = case messageClass of
  MCDiagnostic {} -> original flags messageClass location doc
  MCFatal -> original flags messageClass location doc
  _ -> message `seq` modifyIORef' ref (capture wanted message)
  where
    message :: T.Text
    -- The style GHC itself dumps in, which with -dppr-debug is the debug
    -- style the reader expects (variables printed with their unique).
    message = T.pack (renderWithContext (log_default_dump_context flags) doc)

-- | A missed-specialisation warning as GHC logged it: which flag, the file
-- GHC names for it (it names no line or column) and its text.
data RawMissed = RawMissed
  { rawKind :: !MissedKind,
    rawFile :: !FilePath,
    rawMessage :: !T.Text
  }
  deriving stock (Eq, Show)

-- | A log hook that keeps the warnings of the two missed-specialisation flags,
-- classified by the flag in the warning's reason, and passes every other
-- message on to the hook that was there before.
missedHook :: IORef [RawMissed] -> LogAction -> LogAction
missedHook ref original flags messageClass location doc = case messageClass of
  MCDiagnostic _ (ResolvedDiagnosticReason (WarningWithFlags reasons)) _
    | Just kind <- kindOf (toList reasons) -> message `seq` modifyIORef' ref (RawMissed kind file message :)
  _ -> original flags messageClass location doc
  where
    message :: T.Text
    message = T.strip (T.pack (renderWithContext (log_default_dump_context flags) doc))
    file :: FilePath
    file = case location of
      RealSrcSpan rs _ -> unpackFS (srcSpanFile rs)
      UnhelpfulSpan (UnhelpfulOther name) -> unpackFS name
      UnhelpfulSpan _ -> ""
    kindOf :: [WarningFlag] -> Maybe MissedKind
    kindOf fs
      | Opt_WarnMissedSpecs `elem` fs = Just MissedSpecialisation
      | Opt_WarnAllMissedSpecs `elem` fs = Just MissedSpecialisationAll
      | otherwise = Nothing

-- | The spans of the errors that have one. An error without a real source
-- span is left out; the caller counts it from the total.
errorSpans :: FilePath -> [MsgEnvelope GhcMessage] -> Either InsightError [Span]
errorSpans file envs = traverse (convertSpan file) [rs | env <- envs, RealSrcSpan rs _ <- [errMsgSpan env]]
