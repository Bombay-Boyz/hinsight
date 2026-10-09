-- |
-- Module      : HInsight.Cli
-- Description : Command-line argument handling for the demo program.
--
-- Pure, so it is tested without running anything. The demo's @Main@ performs
-- the effects.
module HInsight.Cli
  ( Command (..),
    parseCommand,
    usageText,
  )
where

import Data.List (isPrefixOf)
import Data.Text (Text)
import Data.Text qualified as T

-- | What the user asked the demo to do.
data Command
  = ShowHelp
  | -- | Analyse this file.
    Analyse FilePath
  | -- | The arguments were unusable; the text says why.
    BadUsage Text
  deriving stock (Eq, Show)

-- | Interpret the arguments. A help flag anywhere wins; otherwise exactly one
-- argument that is not a flag is a file.
parseCommand :: [String] -> Command
parseCommand args
  | any (`elem` ["-h", "--help"]) args = ShowHelp
  | otherwise = case args of
      [] -> BadUsage "no file was given"
      [a]
        | "-" `isPrefixOf` a -> BadUsage ("unknown option " <> T.pack a)
        | otherwise -> Analyse a
      _ -> BadUsage ("expected exactly one file but got " <> T.pack (show (length args)))

-- | The help text, ending in a newline.
usageText :: Text
usageText =
  T.unlines
    [ "Usage: hinsight-demo FILE.hs",
      "",
      "Typechecks one Haskell module with GHC 9.10.3 and reports type",
      "mismatches and typed-hole fits. The module may import installed",
      "packages only.",
      "",
      "GHC's library directory is taken from HINSIGHT_GHC_LIBDIR if set,",
      "otherwise from `ghc-9.10.3 --print-libdir`.",
      "",
      "Try: cabal run hinsight-demo -- test/fixtures/Hole.hs"
    ]
