-- |
-- Module      : Main
-- Description : A small front end that shows what hinsight reports for a file.
module Main (main) where

import Control.Exception (IOException, try)
import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import HInsight (analyseFile, mkLibDir, mkSourceFile, renderError)
import HInsight.Cli (Command (..), parseCommand, usageText)
import HInsight.Error (renderDomainError)
import HInsight.Ghc.Session (ghcInsight)
import HInsight.Report (renderAnalysis)
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hSetEncoding, stderr, stdout, utf8)
import System.Process (readProcess)

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
  found <- findLibDir
  dir <- orExit (found >>= first renderDomainError . mkLibDir)
  file <- orExit (first renderDomainError (mkSourceFile path))
  result <- analyseFile (ghcInsight dir) file
  either (failWith (ExitFailure 1) . renderError) (TIO.putStr . renderAnalysis) result

-- | The library directory: the environment variable if set, otherwise what the
-- pinned compiler reports. This is the front end's job; the library takes the
-- directory as an argument.
findLibDir :: IO (Either Text FilePath)
findLibDir = lookupEnv "HINSIGHT_GHC_LIBDIR" >>= maybe fromPinnedGhc (pure . Right)
  where
    fromPinnedGhc :: IO (Either Text FilePath)
    fromPinnedGhc = do
      result <- try @IOException (readProcess "ghc-9.10.3" ["--print-libdir"] "")
      pure (either (const (Left missingGhc)) (Right . takeWhile (/= '\n')) result)
    missingGhc :: Text
    missingGhc = "could not run ghc-9.10.3; install it with `ghcup install ghc 9.10.3` or set HINSIGHT_GHC_LIBDIR"

orExit :: Either Text a -> IO a
orExit = either (failWith (ExitFailure 2)) pure

failWith :: ExitCode -> Text -> IO a
failWith code message = TIO.hPutStrLn stderr message *> exitWith code
