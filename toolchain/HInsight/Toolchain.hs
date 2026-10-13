-- |
-- Module      : HInsight.Toolchain
-- Description : Finding the GHC 9.10.3 library directory that hinsight needs.
--
-- Everything that runs hinsight (the demos, the tests, the benchmark) needs
-- the library directory of the GHC the package was built with. This is the one
-- place that finds it, so there is one rule rather than several copies of it.
module HInsight.Toolchain
  ( findGhcLibDir,
    missingGhcMessage,
  )
where

import Control.Exception (IOException, try)
import Data.Text (Text)
import System.Environment (lookupEnv)
import System.Process (readProcess)

-- | The library directory to analyse with: @HINSIGHT_GHC_LIBDIR@ if it is set
-- and not empty, otherwise what @ghc-9.10.3 --print-libdir@ reports.
-- 'Nothing' means neither worked; 'missingGhcMessage' says what to do.
findGhcLibDir :: IO (Maybe FilePath)
findGhcLibDir =
  lookupEnv "HINSIGHT_GHC_LIBDIR" >>= \case
    Just dir | not (null dir) -> pure (Just dir)
    _ -> fromPinnedGhc
  where
    fromPinnedGhc :: IO (Maybe FilePath)
    fromPinnedGhc = do
      result <- try @IOException (readProcess "ghc-9.10.3" ["--print-libdir"] "")
      pure (either (const Nothing) nonEmpty result)
    nonEmpty :: String -> Maybe FilePath
    nonEmpty out = case takeWhile (/= '\n') out of
      "" -> Nothing
      dir -> Just dir

-- | What to tell a person when 'findGhcLibDir' found nothing.
missingGhcMessage :: Text
missingGhcMessage =
  "could not run ghc-9.10.3; install it with `ghcup install ghc 9.10.3` or set HINSIGHT_GHC_LIBDIR"
