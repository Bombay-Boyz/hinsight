-- |
-- Module      : HInsight.Environment
-- Description : What a project tells GHC about how its files are compiled.
--
-- An 'Environment' is the one representation every source of project knowledge
-- funnels into (Standard 2.1): the component a file belongs to, the directory
-- that component is rooted in, and the flags GHC compiles it with. Only values
-- that satisfy their invariants can be built (Standard 1.3): the constructors
-- are not exported.
--
-- Nothing here runs a build tool or touches GHC. That is the job of the
-- adapters that produce an 'Environment' and of "HInsight.Ghc" modules that
-- apply one.
module HInsight.Environment
  ( ComponentName,
    mkComponentName,
    unComponentName,
    ProjectRoot,
    mkProjectRoot,
    projectRootPath,
    GhcFlag,
    mkGhcFlag,
    unGhcFlag,
    GhcVersion,
    mkGhcVersion,
    parseGhcVersion,
    renderGhcVersion,
    pinnedGhcVersion,
    Environment,
    mkEnvironment,
    environmentComponent,
    environmentRoot,
    environmentFlags,
    SessionFlags (..),
    sessionFlags,
  )
where

import Data.List (intercalate, partition)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import HInsight.Error (DomainError (..), EnvironmentError (..))
import System.FilePath (isAbsolute)

-- | The name of a build component, such as @hinsight:lib:hinsight@. It is
-- shown to people and compared; its form is the build tool's. Invariant: not
-- blank, and stored without surrounding whitespace.
newtype ComponentName = UnsafeComponentName Text
  deriving stock (Eq, Ord, Show)

-- | Build a 'ComponentName'; the text must not be blank.
mkComponentName :: Text -> Either DomainError ComponentName
mkComponentName t
  | T.null stripped = Left (BlankText "component name")
  | otherwise = Right (UnsafeComponentName stripped)
  where
    stripped :: Text
    stripped = T.strip t

-- | The text inside a 'ComponentName'.
unComponentName :: ComponentName -> Text
unComponentName (UnsafeComponentName t) = t

-- | The directory a component's relative paths are relative to; usually the
-- folder that holds its @.cabal@ file. Invariant: an absolute path, so that
-- it means the same thing wherever the process is running.
newtype ProjectRoot = UnsafeProjectRoot FilePath
  deriving stock (Eq, Ord, Show)

-- | Build a 'ProjectRoot'; the path must be absolute.
mkProjectRoot :: FilePath -> Either DomainError ProjectRoot
mkProjectRoot p
  | null p = Left (EmptyPath "project root")
  | isAbsolute p = Right (UnsafeProjectRoot p)
  | otherwise = Left (RelativeRoot p)

-- | The path inside a 'ProjectRoot'.
projectRootPath :: ProjectRoot -> FilePath
projectRootPath (UnsafeProjectRoot p) = p

-- | One argument for GHC, exactly as a build tool produced it. An argument may
-- contain spaces, since arguments are not split again. Invariant: not empty
-- and without a NUL character, which no process argument can hold.
newtype GhcFlag = UnsafeGhcFlag Text
  deriving stock (Eq, Ord, Show)

-- | Build a 'GhcFlag'.
mkGhcFlag :: Text -> Either DomainError GhcFlag
mkGhcFlag t
  | T.null t || T.any (== '\NUL') t = Left (InvalidFlag t)
  | otherwise = Right (UnsafeGhcFlag t)

-- | The text inside a 'GhcFlag'.
unGhcFlag :: GhcFlag -> Text
unGhcFlag (UnsafeGhcFlag t) = t

-- | A compiler version: one or more non-negative numbers, such as 9.10.3.
-- Compared as written: 9.10 is not 9.10.0.
newtype GhcVersion = UnsafeGhcVersion [Int]
  deriving stock (Eq, Ord, Show)

-- | Build a 'GhcVersion' from its numbers; there must be at least one and none
-- may be negative.
mkGhcVersion :: [Int] -> Either DomainError GhcVersion
mkGhcVersion parts
  | null parts || any (< 0) parts = Left (MalformedVersion (renderParts parts))
  | otherwise = Right (UnsafeGhcVersion parts)

-- | Read a version written as dotted decimal numbers, as @ghc --numeric-version@
-- prints it. Surrounding whitespace is ignored.
parseGhcVersion :: Text -> Either DomainError GhcVersion
parseGhcVersion t = case traverse number (T.splitOn "." (T.strip t)) of
  Nothing -> Left (MalformedVersion t)
  Just parts -> either (const (Left (MalformedVersion t))) Right (mkGhcVersion parts)
  where
    -- Read as an Integer first, so that a number too large for an Int is
    -- refused instead of wrapping round.
    number :: Text -> Maybe Int
    number part = case TR.decimal @Integer part of
      Right (n, rest) | T.null rest && n <= toInteger (maxBound :: Int) -> Just (fromInteger n)
      _ -> Nothing

-- | A version as dotted decimal numbers; 'parseGhcVersion' reads it back.
renderGhcVersion :: GhcVersion -> Text
renderGhcVersion (UnsafeGhcVersion parts) = renderParts parts

renderParts :: [Int] -> Text
renderParts = T.pack . intercalate "." . map show

-- | The one compiler this package supports.
pinnedGhcVersion :: GhcVersion
pinnedGhcVersion = UnsafeGhcVersion [9, 10, 3]

-- | How one component is compiled. Invariant: it was captured for
-- 'pinnedGhcVersion', so a holder never needs to check that again.
data Environment = UnsafeEnvironment
  { environmentComponent :: !ComponentName,
    environmentRoot :: !ProjectRoot,
    environmentFlags :: ![GhcFlag]
  }
  deriving stock (Eq, Show)

-- | Build an 'Environment' from what a build tool reported. The version is the
-- compiler the flags were captured for; flags for any other compiler are
-- refused, because the package databases and interface files they point at
-- cannot be read by the GHC this package links.
mkEnvironment :: ComponentName -> ProjectRoot -> GhcVersion -> [GhcFlag] -> Either EnvironmentError Environment
mkEnvironment component root version flags
  | version == pinnedGhcVersion = Right (UnsafeEnvironment component root flags)
  | otherwise = Left (CompilerMismatch (renderGhcVersion version) (renderGhcVersion pinnedGhcVersion))

-- | The arguments to give a session for an 'Environment', and what was left
-- out on the way.
data SessionFlags = SessionFlags
  { -- | In order: the working directory first, then the environment's flags
    -- that were kept.
    sessionArguments :: ![GhcFlag],
    -- | The environment's flags that were not passed on, in their order.
    sessionDropped :: ![GhcFlag]
  }
  deriving stock (Eq, Show)

-- | The arguments for a session. Three kinds of flag are left out, and reported
-- in 'sessionDropped' rather than lost, because they would change what the
-- analysis sees or write into the project:
--
-- * @-Werror@ and @-Werror=...@, which turn warnings into errors and so change
--   which errors there are to explain;
-- * @-fdefer-type-errors@, @-fdefer-typed-holes@ and
--   @-fdefer-out-of-scope-variables@, which hide the errors that are explained;
-- * @-fwrite-ide-info@, @-fwrite-interface@ and @-fwrite-if-simplified-core@,
--   which make GHC write files into the project's build folders (prior-art
--   class 11), where an analysis must leave nothing behind.
--
-- The arguments start with @-package-env=-@ after the working directory, so a
-- @.ghc.environment@ file in the directory the process runs in cannot add
-- packages to the session.
--
-- The working directory is given with GHC's own @-working-dir@, which offsets
-- every relative path in the flags (@-i@, @-I@, output folders and input
-- files), so no path is rewritten and the process's directory is never changed.
--
-- Limit: an argument that is itself written as @-Werror@ or one of the defer
-- flags after a flag that takes a separate argument (such as @-optc -Werror@)
-- would be dropped too. Build tools write such arguments joined to their flag,
-- and a session that generates no code has no use for the C compiler's
-- options, so this is accepted.
sessionFlags :: Environment -> SessionFlags
sessionFlags env =
  SessionFlags
    { sessionArguments = workingDir <> isolation <> kept,
      sessionDropped = dropped
    }
  where
    workingDir :: [GhcFlag]
    workingDir = [UnsafeGhcFlag "-working-dir", UnsafeGhcFlag (T.pack (projectRootPath (environmentRoot env)))]
    -- GHC reads @.ghc.environment@ files from the process's directory unless
    -- told not to, which would add packages the project never asked for.
    isolation :: [GhcFlag]
    isolation = [UnsafeGhcFlag "-package-env=-"]
    kept, dropped :: [GhcFlag]
    (dropped, kept) = partition changesDiagnostics (environmentFlags env)

-- | Whether a flag would change which errors GHC reports, or make it write
-- files.
changesDiagnostics :: GhcFlag -> Bool
changesDiagnostics (UnsafeGhcFlag t) =
  t == "-Werror"
    || "-Werror=" `T.isPrefixOf` t
    || t `elem` ["-fdefer-type-errors", "-fdefer-typed-holes", "-fdefer-out-of-scope-variables"]
    || t `elem` ["-fwrite-ide-info", "-fwrite-interface", "-fwrite-if-simplified-core"]
