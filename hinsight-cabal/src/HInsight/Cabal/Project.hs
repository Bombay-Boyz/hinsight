-- |
-- Module      : HInsight.Cabal.Project
-- Description : Find the cabal project a file belongs to.
module HInsight.Cabal.Project
  ( findProjectDir,
  )
where

import Control.Monad (filterM)
import System.Directory (canonicalizePath, doesFileExist, listDirectory)
import System.FilePath (takeDirectory, takeExtension, (</>))

-- | The folder cabal would take as the project for this file: the nearest
-- folder above it holding a @cabal.project@, and when there is none, the
-- nearest one holding a @.cabal@ file. This is cabal's own rule, and the one
-- the build-tool library uses, so the two cannot disagree about which project
-- a file is in. 'Nothing' when there is no such folder.
findProjectDir :: FilePath -> IO (Maybe FilePath)
findProjectDir file = do
  start <- takeDirectory <$> canonicalizePath file
  byProject <- upwards (\dir -> doesFileExist (dir </> "cabal.project")) start
  maybe (upwards hasCabalFile start) (pure . Just) byProject

hasCabalFile :: FilePath -> IO Bool
hasCabalFile dir = do
  entries <- listDirectory dir
  any (\entry -> takeExtension entry == ".cabal") <$> filterM (\entry -> doesFileExist (dir </> entry)) entries

-- | The nearest folder, starting at this one and going to the root, that
-- satisfies the check.
upwards :: (FilePath -> IO Bool) -> FilePath -> IO (Maybe FilePath)
upwards holds dir = do
  here <- holds dir
  let parent = takeDirectory dir
  if here
    then pure (Just dir)
    else
      if parent == dir
        then pure Nothing
        else upwards holds parent
