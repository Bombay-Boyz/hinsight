-- |
-- Module      : Main
-- Description : Analyse a file with hinsight and draw what it finds with Tadka.
module Main (main) where

import Control.Exception (IOException, try)
import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8')
import Data.Text.IO qualified as TIO
import HInsight (analyseFile, mkLibDir, mkSourceFile, renderError)
import HInsight.Error (renderDomainError)
import HInsight.Ghc.Session (ghcInsight)
import HInsight.Tadka (findings, namedSource, renderAdaptError, renderExtras)
import HInsight.Tadka.Cli (Command (..), Format (..), parseCommand, usageText)
import HInsight.Toolchain (findGhcLibDir, missingGhcMessage)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hSetEncoding, stderr, stdout, utf8)
import Tadka qualified as Td

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  getArgs >>= run . parseCommand

run :: Command -> IO ()
run = \case
  ShowHelp -> TIO.putStr usageText
  BadUsage why -> failWith (ExitFailure 2) (why <> "\n\n" <> usageText)
  Run format path -> analyse format path

analyse :: Format -> FilePath -> IO ()
analyse format path = do
  found <- findGhcLibDir
  dir <- orExit (maybe (Left missingGhcMessage) (first renderDomainError . mkLibDir) found)
  file <- orExit (first renderDomainError (mkSourceFile path))
  result <- analyseFile (ghcInsight dir) file
  analysis <- either (failWith (ExitFailure 1) . renderError) pure result
  text <- readSource path
  src <- orExit (first renderAdaptError (namedSource path text))
  drawn <- orExit (first renderAdaptError (findings src analysis))
  let config = Td.withTarget (target format) Td.defaultConfig
      extras = renderExtras analysis
  mapM_ (Td.reportDiagnostic config) drawn
  if null drawn && T.null extras
    then TIO.putStrLn "nothing to report: no type mismatches, typed holes or pipelines found"
    else case format of
      -- Standard output stays a stream of JSON objects.
      Json -> TIO.hPutStr stderr extras
      _ -> TIO.putStr extras

target :: Format -> Td.Target
target = \case
  Graphical -> Td.TGraphical
  Narratable -> Td.TNarratable
  Json -> Td.TJson

-- | The file as GHC read it: UTF-8, whatever the locale says.
readSource :: FilePath -> IO Text
readSource path = do
  bytes <- try @IOException (BS.readFile path)
  either (failWith (ExitFailure 1) . T.pack . show) decode bytes
  where
    decode :: BS.ByteString -> IO Text
    decode = either (const (failWith (ExitFailure 1) (T.pack path <> " is not valid UTF-8"))) pure . decodeUtf8'

orExit :: Either Text a -> IO a
orExit = either (failWith (ExitFailure 2)) pure

failWith :: ExitCode -> Text -> IO a
failWith code message = TIO.hPutStrLn stderr message *> exitWith code
