module Hob.DirectoryTree where

import Control.Monad   (forM)
import Data.Tree
import System.FilePath

newtype IsDirectory = IsDirectory Bool

data DirectoryTreeElement = DirectoryTreeElement String FilePath IsDirectory

directoryTreeElementLabel :: DirectoryTreeElement -> String
directoryTreeElementLabel (DirectoryTreeElement label _ _) = label

directoryTreeElementPath :: DirectoryTreeElement -> FilePath
directoryTreeElementPath (DirectoryTreeElement _ path _) = path

directoryTreeElementIsDirectory :: DirectoryTreeElement -> Bool
directoryTreeElementIsDirectory (DirectoryTreeElement _ _ (IsDirectory isDir)) = isDir

instance Show (DirectoryTreeElement) where
  show (DirectoryTreeElement label path (IsDirectory isDir)) = show label ++" ["++ show path ++"] "++(if isDir then "(directory)" else "(file)")

instance  Eq (DirectoryTreeElement)  where
    (DirectoryTreeElement label path (IsDirectory isDir)) == (DirectoryTreeElement label' path' (IsDirectory isDir')) = (label == label') && (path == path') && (isDir == isDir')

type DirectoryTreeLoader = FilePath -> IO (Forest DirectoryTreeElement)
type DirectoryReader = FilePath -> IO [FilePath]
type IsDirectoryCheck = FilePath -> IO Bool

fileTreeGenerator :: DirectoryReader -> IsDirectoryCheck -> DirectoryTreeLoader
fileTreeGenerator getDirectoryContents doesDirectoryExist root = do
    directoryContents <- getFilteredDirectoryContents
    forM directoryContents $ \ child -> do
        let childPath = root </> child
        isDir <- doesDirectoryExist childPath
        childrenForest <- if isDir then callSelf childPath else return []
        return $ Node (DirectoryTreeElement child childPath (IsDirectory isDir)) childrenForest
    where
        callSelf = fileTreeGenerator getDirectoryContents doesDirectoryExist
        removeDotDirectories = filter (\child -> not ((child == ".") || (child == "..")))
        getFilteredDirectoryContents = do
            contents <- getDirectoryContents root
            return (removeDotDirectories contents)
