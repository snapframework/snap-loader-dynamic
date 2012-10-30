module Snap.Loader.Dynamic.TreeWatcher
  ( TreeStatus
  , getTreeStatus
  , checkTreeStatus
  ) where

------------------------------------------------------------------------------
import Control.Applicative
import Data.Time.Clock
import System.Directory
import System.Directory.Tree


------------------------------------------------------------------------------
-- | An opaque representation of the contents and last modification
-- times of a forest of directory trees.
data TreeStatus = TS [FilePath] [AnchoredDirTree UTCTime]


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
readModificationTimes :: [FilePath] -> IO [AnchoredDirTree UTCTime]
readModificationTimes = mapM $ readDirectoryWith getModificationTime
