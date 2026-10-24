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

import Data.List (isPrefixOf, partition)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import HInsight.Chain (ChainView (..))

-- | What the user asked the demo to do.
data Command
  = ShowHelp
  | -- | Analyse this file.
    Analyse FilePath
  | -- | Show what GHC logged on the way to each error of this file, in this
    -- view. This runs GHC a second time with its trace on.
    ExplainChains ChainView FilePath
  | -- | The arguments were unusable; the text says why.
    BadUsage Text
  deriving stock (Eq, Show)

-- | Interpret the arguments. A help flag anywhere wins; otherwise exactly one
-- argument that is not a flag is a file. @--chains@ asks for the chains that
-- follow the variables of the error, and @--chains=whole@ for every record of
-- the binding; the last of them wins.
parseCommand :: [String] -> Command
parseCommand args
  | any (`elem` ["-h", "--help"]) args = ShowHelp
  | otherwise = case partition ("-" `isPrefixOf`) args of
      (flags, files) -> case traverse chainFlag flags of
        Left unknown -> BadUsage ("unknown option " <> T.pack unknown)
        Right views -> case files of
          [] -> BadUsage "no file was given"
          [file] -> maybe (Analyse file) (`ExplainChains` file) (lastOf views)
          _ -> BadUsage ("expected exactly one file but got " <> T.pack (show (length files)))
  where
    chainFlag :: String -> Either String ChainView
    chainFlag = \case
      "--chains" -> Right FollowingVariables
      "--chains=whole" -> Right WholeBinding
      other -> Left other
    lastOf :: [a] -> Maybe a
    lastOf = listToMaybe . reverse

-- | The help text, ending in a newline.
usageText :: Text
usageText =
  T.unlines
    [ "Usage: hinsight-demo [--chains | --chains=whole] FILE.hs",
      "",
      "With --chains it also shows what GHC logged on the way to each type",
      "error, following the type variables of the error; --chains=whole",
      "shows every record of the binding. This runs GHC a second time with",
      "its trace on, which takes several times as long.",
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
