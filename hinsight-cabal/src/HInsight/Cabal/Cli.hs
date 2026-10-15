-- |
-- Module      : HInsight.Cabal.Cli
-- Description : Command-line arguments of the demo, parsed without any IO.
module HInsight.Cabal.Cli
  ( EnvironmentChoice (..),
    Command (..),
    parseCommand,
    usageText,
  )
where

import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T

-- | Where the file's project comes from.
data EnvironmentChoice
  = -- | Ask cabal how the file's component is compiled.
    CabalEnvironment
  | -- | Work it out from the file's header and place; packages are not
    -- resolved.
    HeaderEnvironment
  deriving stock (Eq, Show, Enum, Bounded)

data Command
  = ShowHelp
  | -- | Analyse this file in this environment.
    Run EnvironmentChoice FilePath
  | -- | The arguments were unusable; the text says why.
    BadUsage Text
  deriving stock (Eq, Show)

-- | Read the arguments. Help wins over everything else. The environment
-- defaults to 'CabalEnvironment' and may be given once, as
-- @--environment NAME@ or @--environment=NAME@.
parseCommand :: [String] -> Command
parseCommand args
  | any (`elem` ["-h", "--help"]) args = ShowHelp
  | otherwise = go Nothing Nothing args
  where
    go :: Maybe EnvironmentChoice -> Maybe FilePath -> [String] -> Command
    go choice file = \case
      [] -> case file of
        Nothing -> BadUsage "no file was given"
        Just f -> Run (fromMaybe CabalEnvironment choice) f
      "--environment" : rest -> case rest of
        [] -> BadUsage "--environment needs a value"
        v : more -> withChoice v more
      a : rest
        | Just v <- stripPrefix "--environment=" a -> withChoice v rest
        | "-" `isPrefixOf` a -> BadUsage ("unknown option " <> T.pack a)
        | Just _ <- file -> BadUsage "expected exactly one file but got more"
        | otherwise -> go choice (Just a) rest
      where
        withChoice :: String -> [String] -> Command
        withChoice v more = case (choice, choiceNamed v) of
          (Just _, _) -> BadUsage "--environment was given twice"
          (Nothing, Nothing) -> BadUsage ("unknown environment " <> T.pack v <> "; use cabal or header")
          (Nothing, Just c) -> go (Just c) file more

choiceNamed :: String -> Maybe EnvironmentChoice
choiceNamed = \case
  "cabal" -> Just CabalEnvironment
  "header" -> Just HeaderEnvironment
  _ -> Nothing

usageText :: Text
usageText =
  T.unlines
    [ "Usage: hinsight-cabal-demo [--environment cabal|header] FILE.hs",
      "",
      "Typechecks one Haskell module with GHC 9.10.3 and reports type",
      "mismatches, typed-hole fits and type flow.",
      "",
      "  cabal   ask cabal how the file's component is compiled, so its",
      "          extensions, flags and packages apply (the default; the",
      "          project must pin GHC 9.10.3 with `with-compiler`)",
      "  header  work the project out from the file itself; packages that",
      "          are not installed globally are not found",
      "",
      "The report ends with the environment the file was analysed in and the",
      "flags that were left out of the session (-Werror, flags that defer",
      "errors, and flags that make GHC write files).",
      "",
      "GHC's library directory is taken from HINSIGHT_GHC_LIBDIR if set,",
      "otherwise from `ghc-9.10.3 --print-libdir`.",
      "",
      "Try: cabal run hinsight-cabal-demo -- src/HInsight/Environment.hs"
    ]
