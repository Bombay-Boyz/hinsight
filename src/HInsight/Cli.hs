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
import HInsight.Core (Optimisation (..))

-- | What the user asked the demo to do.
data Command
  = ShowHelp
  | -- | Analyse this file.
    Analyse FilePath
  | -- | Show what GHC logged on the way to each error of this file, in this
    -- view. This runs GHC a second time with its trace on.
    ExplainChains ChainView FilePath
  | -- | Show what GHC says each splice of this file became. This runs GHC a
    -- second time with its splice dump on.
    ExplainSplices FilePath
  | -- | Show what GHC says about the Core it made for this file, at this
    -- optimisation. This compiles the file past the typechecker.
    ExplainCore Optimisation FilePath
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
      (flags, files) -> case traverse readFlag flags of
        Left unknown -> BadUsage ("unknown option " <> T.pack unknown)
        Right parsed -> case files of
          [] -> BadUsage "no file was given"
          [file] -> forFile file parsed
          _ -> BadUsage ("expected exactly one file but got " <> T.pack (show (length files)))
  where
    readFlag :: String -> Either String Flag
    readFlag = \case
      "--chains" -> Right (ChainsFlag FollowingVariables)
      "--chains=whole" -> Right (ChainsFlag WholeBinding)
      "--splices" -> Right SplicesFlag
      "--core" -> Right (CoreFlag Optimised)
      "--core=unoptimised" -> Right (CoreFlag Unoptimised)
      other -> Left other
    forFile :: FilePath -> [Flag] -> Command
    forFile file parsed = case (views, SplicesFlag `elem` parsed, levels) of
      ([], False, []) -> Analyse file
      (_ : _, False, []) -> maybe (Analyse file) (`ExplainChains` file) (lastOf views)
      ([], True, []) -> ExplainSplices file
      ([], False, _ : _) -> maybe (Analyse file) (`ExplainCore` file) (lastOf levels)
      _ -> BadUsage "choose one of --chains, --splices and --core"
      where
        views :: [ChainView]
        views = [v | ChainsFlag v <- parsed]
        levels :: [Optimisation]
        levels = [o | CoreFlag o <- parsed]
    lastOf :: [a] -> Maybe a
    lastOf = listToMaybe . reverse

-- | A flag the demo knows.
data Flag
  = ChainsFlag ChainView
  | SplicesFlag
  | CoreFlag Optimisation
  deriving stock (Eq)

-- | The help text, ending in a newline.
usageText :: Text
usageText =
  T.unlines
    [ "Usage: hinsight-demo [--chains | --chains=whole | --splices | --core | --core=unoptimised] FILE.hs",
      "",
      "With --chains it also shows what GHC logged on the way to each type",
      "error, following the type variables of the error; --chains=whole",
      "shows every record of the binding. This runs GHC a second time with",
      "its trace on, which takes several times as long.",
      "",
      "With --splices it shows what GHC says each Template Haskell splice",
      "of the file became (kind, code, result). This runs GHC a second time",
      "with its splice dump on.",
      "",
      "With --core it shows what GHC logged for the Core of the file: the",
      "rules that fired and the Core before and after the simplifier, at -O",
      "(--core=unoptimised for -O0). This compiles the file past the",
      "typechecker; anything GHC writes goes to a temporary folder that is",
      "removed afterwards.",
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
