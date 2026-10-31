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
    coreFlags,
    stgBuildFlags,
    stgDumpFlags,
    spliceFlags,
    enableFlags,
    captureHook,
    errorSpans,
  )
where

import Control.Monad (void)
import Data.IORef (IORef, modifyIORef')
import Data.Text qualified as T
import GHC (Ghc, getLogger, getSessionDynFlags, parseDynamicFlags, setSessionDynFlags)
import GHC.Driver.Errors.Types (GhcMessage)
import GHC.Types.Error (MessageClass (..), MsgEnvelope (..))
import GHC.Types.SrcLoc (SrcSpan (..), noLoc, unLoc)
import GHC.Utils.Logger (LogAction, LogFlags (..))
import GHC.Utils.Outputable (renderWithContext)
import HInsight.Core (Optimisation (..))
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

-- | The flags of the Core dumps, and a folder for anything GHC writes while it
-- makes Core. The folder is the caller's to remove.
coreFlags :: Optimisation -> FilePath -> [String]
coreFlags optimisation folder = optimisationFlag optimisation : coreDumps <> folderFlags folder

-- | The flags of a compile with no dump on, so the dependencies of a file can
-- be compiled without logging anything.
stgBuildFlags :: Optimisation -> FilePath -> [String]
stgBuildFlags optimisation folder = optimisationFlag optimisation : folderFlags folder

-- | The dumps to switch on once the dependencies are compiled.
stgDumpFlags :: [String]
stgDumpFlags = coreDumps <> ["-ddump-stg-final"]

optimisationFlag :: Optimisation -> String
optimisationFlag = \case
  Optimised -> "-O"
  Unoptimised -> "-O0"

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

-- | The spans of the errors that have one. An error without a real source
-- span is left out; the caller counts it from the total.
errorSpans :: FilePath -> [MsgEnvelope GhcMessage] -> Either InsightError [Span]
errorSpans file envs = traverse (convertSpan file) [rs | env <- envs, RealSrcSpan rs _ <- [errMsgSpan env]]
