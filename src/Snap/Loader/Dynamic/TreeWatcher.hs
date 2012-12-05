module Snap.Loader.Dynamic.TreeWatcher
  ( TreeStatus
  , getTreeStatus
  , checkTreeStatus
  ) where

#ifndef MIN_VERSION_directory
#define MIN_VERSION_directory(x,y,z) 1
#endif

------------------------------------------------------------------------------
import Control.Applicative
import System.Directory
import System.Directory.Tree

#if MIN_VERSION_directory(1,2,0)
import Data.Time.Clock
#else
import System.Time
#endif


------------------------------------------------------------------------------
-- | An opaque representation of the contents and last modification
-- times of a forest of directory trees.
#if MIN_VERSION_directory(1,2,0)
data TreeStatus = TS [FilePath] [AnchoredDirTree UTCTime]
#else
data TreeStatus = TS [FilePath] [AnchoredDirTree ClockTime]
#endif


------------------------------------------------------------------------------
-- | Create a 'TreeStatus' for later checking with 'checkTreeStatus'
getTreeStatus :: [FilePath] -> IO TreeStatus
getTreeStatus = liftA2 (<$>) TS readModificationTimes


------------------------------------------------------------------------------
-- | Checks that all the files present in the initial set of paths are
-- the exact set of files currently present, with unchanged modifcations times
checkTreeStatus :: TreeStatus -> IO Bool
checkTreeStatus (TS paths entries) = check <$> readModificationTimes paths
  where
    check = and . zipWith (==) entries


------------------------------------------------------------------------------
-- | This is the core of the functions in this module.  It converts a
-- list of filepaths into a list of 'AnchoredDirTree' annotated with
-- the modification times of the files located in those paths.
#if MIN_VERSION_directory(1,2,0)
readModificationTimes :: [FilePath] -> IO [AnchoredDirTree UTCTime]
#else
readModificationTimes :: [FilePath] -> IO [AnchoredDirTree ClockTime]
#endif
readModificationTimes = mapM $ readDirectoryWith getModificationTime
