-- |
-- Module      : HInsight.Project
-- Description : Where the source folder of a project is, worked out from one file.
--
-- A file whose header says @module A.B.C@ and that lives at @root/A/B/C.hs@
-- belongs to a project whose modules are found under @root@. Pure path
-- arithmetic; nothing here touches the file system.
module HInsight.Project
  ( sourceRoot,
  )
where

import Data.List (isSuffixOf)
import System.FilePath (dropExtension, joinPath, splitDirectories, takeDirectory)

-- | The folder that module names resolve against, given a file and the module
-- name in its header. When the file's place in the folder tree does not match
-- its module name (a @Main@ module, a script, a misplaced file) the folder
-- holding the file is used, so its siblings can still be imported.
sourceRoot :: FilePath -> String -> FilePath
sourceRoot file moduleName
  | not (null parts) && parts `isSuffixOf` components = joinRoot (take (length components - length parts) components)
  | otherwise = takeDirectory file
  where
    parts :: [String]
    parts = words (map (\c -> if c == '.' then ' ' else c) moduleName)
    components :: [FilePath]
    components = splitDirectories (dropExtension file)
    joinRoot :: [FilePath] -> FilePath
    joinRoot [] = "."
    joinRoot dirs = joinPath dirs
