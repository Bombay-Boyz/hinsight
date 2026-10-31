-- |
-- Module      : Main
-- Description : A small front end that shows what hinsight reports for a file.
module Main (main) where

import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import HInsight (Deep (..), LibDir, SourceFile, analyseFile, mkLibDir, mkSourceFile, renderChains, renderCore, renderError, renderSplices)
import HInsight.Cli (Command (..), parseCommand, usageText)
import HInsight.Error (renderDomainError)
import HInsight.Ghc.Session (ghcDeep, ghcInsight)
import HInsight.Report (renderAnalysis)
import HInsight.Toolchain (findGhcLibDir, missingGhcMessage)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hSetEncoding, stderr, stdout, utf8)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  getArgs >>= run . parseCommand

run :: Command -> IO ()
run = \case
  ShowHelp -> TIO.putStr usageText
  BadUsage why -> failWith (ExitFailure 2) (why <> "\n\n" <> usageText)
  Analyse path -> do
    (dir, file) <- inputs path
    result <- analyseFile (ghcInsight dir) file
    either (failWith (ExitFailure 1) . renderError) (TIO.putStr . renderAnalysis) result
  ExplainChains view path -> do
    (dir, file) <- inputs path
    result <- chainsFor (ghcDeep dir) view file
    either (failWith (ExitFailure 1) . renderError) (TIO.putStr . renderChains) result
  ExplainSplices path -> do
    (dir, file) <- inputs path
    result <- splicesFor (ghcDeep dir) file
    either (failWith (ExitFailure 1) . renderError) (TIO.putStr . renderSplices) result
  ExplainCore request path -> do
    (dir, file) <- inputs path
    result <- coreOf (ghcDeep dir) request file
    either (failWith (ExitFailure 1) . renderError) (TIO.putStr . renderCore) result

-- | GHC's library directory and the file, or the reason they are unusable.
inputs :: FilePath -> IO (LibDir, SourceFile)
inputs path = do
  found <- findGhcLibDir
  dir <- orExit (maybe (Left missingGhcMessage) (first renderDomainError . mkLibDir) found)
  file <- orExit (first renderDomainError (mkSourceFile path))
  pure (dir, file)

orExit :: Either Text a -> IO a
orExit = either (failWith (ExitFailure 2)) pure

failWith :: ExitCode -> Text -> IO a
failWith code message = TIO.hPutStrLn stderr message *> exitWith code
