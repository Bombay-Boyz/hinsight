-- |
-- Module      : Main
-- Description : A small front end that shows what hinsight reports for a file.
module Main (main) where

import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import HInsight (analyseFile, mkLibDir, mkSourceFile, renderError)
import HInsight.Cli (Command (..), parseCommand, usageText)
import HInsight.Error (renderDomainError)
import HInsight.Ghc.Session (ghcInsight)
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
  Analyse path -> analyse path

analyse :: FilePath -> IO ()
analyse path = do
  found <- findGhcLibDir
  dir <- orExit (maybe (Left missingGhcMessage) (first renderDomainError . mkLibDir) found)
  file <- orExit (first renderDomainError (mkSourceFile path))
  result <- analyseFile (ghcInsight dir) file
  either (failWith (ExitFailure 1) . renderError) (TIO.putStr . renderAnalysis) result

orExit :: Either Text a -> IO a
orExit = either (failWith (ExitFailure 2)) pure

failWith :: ExitCode -> Text -> IO a
failWith code message = TIO.hPutStrLn stderr message *> exitWith code
