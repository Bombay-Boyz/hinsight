-- |
-- Module      : HInsight.Ghc.Environment
-- Description : Apply a project environment's flags to a GHC session.
--
-- The pure side ("HInsight.Environment") decides which arguments a session
-- may be given; this module hands them to GHC's own flag parser, so every flag
-- means exactly what it means to @ghc@. A flag GHC refuses, or an argument it
-- takes for a file name, is a typed error rather than something silently
-- ignored: a session that quietly lacks an extension would report errors the
-- project's own build never has.
module HInsight.Ghc.Environment
  ( applyEnvironment,
  )
where

import Control.Exception (try)
import Control.Monad.IO.Class (liftIO)
import Data.Text qualified as T
import GHC (DynFlags, Ghc, getLogger, parseDynamicFlags)
import GHC.Types.SrcLoc (Located, noLoc, unLoc)
import GHC.Utils.Panic (GhcException)
import HInsight.Environment (Environment, sessionArguments, sessionFlags, unGhcFlag)
import HInsight.Error (EnvironmentError (..))

-- | The flags with the environment's arguments applied.
--
-- GHC's warnings about the flags (for example a deprecated one) are not
-- reported: the analysis explains errors, and a flag that works is not one.
applyEnvironment :: Environment -> DynFlags -> Ghc (Either EnvironmentError DynFlags)
applyEnvironment env flags = do
  logger <- getLogger
  parsed <- liftIO (try (parseDynamicFlags logger flags arguments))
  pure $ case parsed of
    Left (thrown :: GhcException) -> Left (FlagRejected (oneLine (show thrown)))
    Right (updated, leftovers, _warnings)
      | null leftovers -> Right updated
      | otherwise -> Left (UnrecognisedArguments (map (T.pack . unLoc) leftovers))
  where
    arguments :: [Located String]
    arguments = map (noLoc . T.unpack . unGhcFlag) (sessionArguments (sessionFlags env))

    oneLine :: String -> T.Text
    oneLine = T.unwords . T.words . T.pack
