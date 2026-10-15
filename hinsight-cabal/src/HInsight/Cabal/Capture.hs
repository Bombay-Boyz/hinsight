-- |
-- Module      : HInsight.Cabal.Capture
-- Description : Turn what cabal reported about a component into an 'Environment'.
--
-- Pure: everything here takes values already read from cabal, so it is tested
-- without cabal. The process that produces them is in "HInsight.Cabal".
module HInsight.Cabal.Capture
  ( Captured (..),
    toEnvironment,
    componentNameOf,
    renderCaptureFailure,
  )
where

import Data.Bifunctor (first)
import Data.List (stripPrefix)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import HInsight.Environment
  ( Environment,
    mkComponentName,
    mkEnvironment,
    mkGhcFlag,
    mkProjectRoot,
    parseGhcVersion,
  )
import HInsight.Error (InsightError (..))
import System.Exit (ExitCode (..))

-- | What cabal reported for one file's component.
data Captured = Captured
  { -- | The component's root folder, absolute.
    capturedRoot :: FilePath,
    -- | The arguments cabal would give GHC for the component, in order.
    capturedOptions :: [String],
    -- | The compiler's version as GHC prints it (@--numeric-version@).
    capturedVersion :: String
  }
  deriving stock (Eq, Show)

-- | Build the environment, refusing a compiler other than the one this package
-- supports ('HInsight.Error.CompilerMismatch').
--
-- Empty arguments are dropped: GHC gives them no meaning and a flag cannot be
-- empty. The component's name is its unit id, or @unnamed@ when cabal did not
-- pass one.
toEnvironment :: Captured -> Either InsightError Environment
toEnvironment captured = do
  root <- first DomainFailure (mkProjectRoot (capturedRoot captured))
  version <- first DomainFailure (parseGhcVersion (T.pack (capturedVersion captured)))
  name <- first DomainFailure (mkComponentName (maybe "unnamed" T.pack (componentNameOf options)))
  flags <- first DomainFailure (traverse (mkGhcFlag . T.pack) options)
  first EnvironmentFailure (mkEnvironment name root version flags)
  where
    options :: [String]
    options = filter (not . null) (capturedOptions captured)

-- | The unit id GHC is told the component is called, if the arguments say. It
-- is written either as @-this-unit-id X@ or @-this-unit-id=X@; the last one
-- wins, as it does for GHC.
componentNameOf :: [String] -> Maybe String
componentNameOf = listToMaybe . reverse . units
  where
    units :: [String] -> [String]
    units = \case
      flag : value : rest | flag == "-this-unit-id" -> value : units rest
      arg : rest
        | Just value <- stripPrefix "-this-unit-id=" arg -> value : units rest
        | otherwise -> units rest
      [] -> []

-- | One line saying why cabal could not be asked: its exit status and what it
-- wrote to standard error.
renderCaptureFailure :: ExitCode -> [String] -> Text
renderCaptureFailure status stderr =
  T.unwords (T.words (T.pack (unwords (cause : stderr))))
  where
    cause :: String
    cause = case status of
      ExitSuccess -> "no flags were produced"
      ExitFailure n -> "exit status " <> show n
