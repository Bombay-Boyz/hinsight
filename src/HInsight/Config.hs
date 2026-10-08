-- |
-- Module      : HInsight.Config
-- Description : The inputs this package is handed.
--
-- Tool locations are arguments, never discovered here: only the caller knows
-- where its tools live.
module HInsight.Config
  ( LibDir,
    mkLibDir,
    libDirPath,
  )
where

import HInsight.Error (DomainError (..))

-- | The directory GHC reports as its library directory (the output of
-- @ghc --print-libdir@). Invariant: not empty. Existence is checked when a
-- session starts, because it is a fact about the machine, not about the value.
newtype LibDir = UnsafeLibDir FilePath
  deriving stock (Eq, Ord, Show)

-- | Build a 'LibDir'; the path must not be empty.
mkLibDir :: FilePath -> Either DomainError LibDir
mkLibDir p
  | null p = Left (EmptyPath "GHC library directory")
  | otherwise = Right (UnsafeLibDir p)

-- | The path inside a 'LibDir'.
libDirPath :: LibDir -> FilePath
libDirPath (UnsafeLibDir p) = p
