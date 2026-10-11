-- |
-- Module      : HInsight.Graph
-- Description : The modules of a project and which of them import which.
--
-- A 'ProjectGraph' holds the home modules reachable from the analysed file
-- and the imports between them. Imports of modules outside the project are not
-- part of it. Cycles are representable, because GHC can be asked about a
-- project that has them; 'importCycles' reports them.
module HInsight.Graph
  ( ModuleName,
    mkModuleName,
    unModuleName,
    ProjectGraph,
    emptyGraph,
    mkProjectGraph,
    graphModules,
    graphImportCount,
    importsOf,
    dependencyOrder,
    importCycles,
    renderGraph,
  )
where

import Data.Graph (flattenSCC, stronglyConnComp)
import Data.List (sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import HInsight.Error (DomainError (..))

-- | The name of a module as written in an import, such as @Data.List@.
-- Invariant: trimmed and not blank.
newtype ModuleName = UnsafeModuleName Text
  deriving stock (Eq, Ord, Show)

-- | Build a 'ModuleName'; surrounding whitespace is dropped and the rest must
-- not be empty.
mkModuleName :: Text -> Either DomainError ModuleName
mkModuleName t
  | T.null trimmed = Left (BlankText "module name")
  | otherwise = Right (UnsafeModuleName trimmed)
  where
    trimmed :: Text
    trimmed = T.strip t

-- | The text inside a 'ModuleName'.
unModuleName :: ModuleName -> Text
unModuleName (UnsafeModuleName t) = t

-- | Each module with the modules it imports. Invariants: every imported module
-- is itself a key, and each import list is sorted and has no repeats.
newtype ProjectGraph = UnsafeProjectGraph (Map ModuleName [ModuleName])
  deriving stock (Eq, Show)

-- | A graph with no modules.
emptyGraph :: ProjectGraph
emptyGraph = UnsafeProjectGraph Map.empty

-- | Build a 'ProjectGraph' from each module and its imports. A module may not
-- be listed twice, and every import must be one of the listed modules. Import
-- lists are sorted and de-duplicated.
mkProjectGraph :: [(ModuleName, [ModuleName])] -> Either DomainError ProjectGraph
mkProjectGraph entries = do
  case duplicate (map fst entries) of
    Just name -> Left (DuplicateModule (unModuleName name))
    Nothing -> Right ()
  mapM_ checkImports entries
  Right (UnsafeProjectGraph (Map.fromList [(m, Set.toAscList (Set.fromList is)) | (m, is) <- entries]))
  where
    known :: Set.Set ModuleName
    known = Set.fromList (map fst entries)
    checkImports :: (ModuleName, [ModuleName]) -> Either DomainError ()
    checkImports (m, is) = case filter (`Set.notMember` known) is of
      [] -> Right ()
      missing : _ -> Left (UnknownImport (unModuleName m) (unModuleName missing))

-- | The first module that occurs twice, if any.
duplicate :: (Ord a) => [a] -> Maybe a
duplicate = go Set.empty
  where
    go :: (Ord a) => Set.Set a -> [a] -> Maybe a
    go _ [] = Nothing
    go seen (x : xs)
      | x `Set.member` seen = Just x
      | otherwise = go (Set.insert x seen) xs

-- | Every module, in name order.
graphModules :: ProjectGraph -> [ModuleName]
graphModules (UnsafeProjectGraph g) = Map.keys g

-- | The number of imports between the modules of the graph.
graphImportCount :: ProjectGraph -> Int
graphImportCount (UnsafeProjectGraph g) = sum (map length (Map.elems g))

-- | The modules that one module imports, in name order. A module that is not
-- in the graph imports nothing.
importsOf :: ModuleName -> ProjectGraph -> [ModuleName]
importsOf m (UnsafeProjectGraph g) = Map.findWithDefault [] m g

-- | Groups of modules, each group after everything it imports. A group has
-- more than one module only when those modules import each other in a cycle.
-- Within a group the modules are in name order.
dependencyOrder :: ProjectGraph -> [[ModuleName]]
dependencyOrder (UnsafeProjectGraph g) =
  map (sort . flattenSCC) (stronglyConnComp [(m, m, is) | (m, is) <- Map.toAscList g])

-- | The groups of modules that import each other in a cycle, including a
-- module that imports itself.
importCycles :: ProjectGraph -> [[ModuleName]]
importCycles g = filter cyclic (dependencyOrder g)
  where
    cyclic :: [ModuleName] -> Bool
    cyclic [m] = m `elem` importsOf m g
    cyclic _ = True

-- | A header with the counts, one line per module in dependency order (with
-- what it imports, when it imports anything), then one line per import cycle.
renderGraph :: ProjectGraph -> Text
renderGraph g =
  T.unlines
    ( header
        : map moduleLine (concat (dependencyOrder g))
          <> map cycleLine (importCycles g)
    )
  where
    header :: Text
    header =
      "module graph: "
        <> count (length (graphModules g)) "module"
        <> ", "
        <> count (graphImportCount g) "import"
    width :: Int
    width = maximum (0 : map (T.length . unModuleName) (graphModules g))
    moduleLine :: ModuleName -> Text
    moduleLine m = case importsOf m g of
      [] -> "  " <> unModuleName m
      is -> "  " <> T.justifyLeft width ' ' (unModuleName m) <> "  imports " <> T.intercalate ", " (map unModuleName is)
    cycleLine :: [ModuleName] -> Text
    cycleLine ms = "import cycle: " <> T.intercalate ", " (map unModuleName ms)

count :: Int -> Text -> Text
count n noun = T.pack (show n) <> " " <> noun <> (if n == 1 then "" else "s")
