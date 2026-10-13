-- |
-- Module      : HInsight.Tadka.Cli
-- Description : Command-line arguments of the demo, parsed without any IO.
module HInsight.Tadka.Cli
  ( Format (..),
    Command (..),
    parseCommand,
    usageText,
  )
where

import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T

-- | How the diagnostics are drawn.
data Format
  = -- | Annotated source with underlines, for a terminal.
    Graphical
  | -- | Plain sentences, for a screen reader.
    Narratable
  | -- | One JSON object per diagnostic, for tools.
    Json
  deriving stock (Eq, Show, Enum, Bounded)

data Command
  = ShowHelp
  | -- | Analyse this file and draw the findings this way.
    Run Format FilePath
  | -- | The arguments were unusable; the text says why.
    BadUsage Text
  deriving stock (Eq, Show)

-- | Read the arguments. Help wins over everything else. The format defaults to
-- 'Graphical' and may be given once, as @--format NAME@ or @--format=NAME@.
parseCommand :: [String] -> Command
parseCommand args
  | any (`elem` ["-h", "--help"]) args = ShowHelp
  | otherwise = go Nothing Nothing args
  where
    go :: Maybe Format -> Maybe FilePath -> [String] -> Command
    go fmt file = \case
      [] -> case file of
        Nothing -> BadUsage "no file was given"
        Just f -> Run (fromMaybe Graphical fmt) f
      "--format" : rest -> case rest of
        [] -> BadUsage "--format needs a value"
        v : more -> withFormat v more
      a : rest
        | Just v <- stripPrefix "--format=" a -> withFormat v rest
        | "-" `isPrefixOf` a -> BadUsage ("unknown option " <> T.pack a)
        | Just _ <- file -> BadUsage "expected exactly one file but got more"
        | otherwise -> go fmt (Just a) rest
      where
        withFormat :: String -> [String] -> Command
        withFormat v more = case (fmt, formatNamed v) of
          (Just _, _) -> BadUsage "--format was given twice"
          (Nothing, Nothing) -> BadUsage ("unknown format " <> T.pack v <> "; use graphical, narratable or json")
          (Nothing, Just f) -> go (Just f) file more

formatNamed :: String -> Maybe Format
formatNamed = \case
  "graphical" -> Just Graphical
  "narratable" -> Just Narratable
  "json" -> Just Json
  _ -> Nothing

usageText :: Text
usageText =
  T.unlines
    [ "Usage: hinsight-tadka-demo [--format graphical|narratable|json] FILE.hs",
      "",
      "Analyses one Haskell module with hinsight (GHC 9.10.3) and draws what it",
      "finds with Tadka: type mismatches and typed holes as errors, and type",
      "flow through . $ and >>= as advice.",
      "",
      "  graphical   annotated source with underlines (the default)",
      "  narratable  plain sentences, one per fact, for a screen reader",
      "  json        one JSON object per diagnostic; the module graph and the",
      "              count of unexplained errors go to standard error",
      "",
      "GHC's library directory is taken from HINSIGHT_GHC_LIBDIR if set,",
      "otherwise from `ghc-9.10.3 --print-libdir`.",
      "",
      "Try: cabal run hinsight-tadka-demo -- test/fixtures/TypeMismatch.hs"
    ]
