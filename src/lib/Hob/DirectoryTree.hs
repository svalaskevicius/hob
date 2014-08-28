module Hob.DirectoryTree where

import Control.Monad   (forM)
import Data.Tree
import System.FilePath

type IsDirectory = Bool

data DirectoryTreeElement = DirectoryTreeElement { elementLabel :: String, elementPath :: FilePath, isDirectory :: IsDirectory }

instance Show (DirectoryTreeElement) where
  show (DirectoryTreeElement label path isDir) = show label ++" ["++ show path ++"] "++(if isDir then "(directory)" else "(file)")

instance  Eq (DirectoryTreeElement)  where
    (DirectoryTreeElement label path isDir) == (DirectoryTreeElement label' path' isDir') = (label == label') && (path == path') && (isDir == isDir')

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
        return $ Node (DirectoryTreeElement child childPath isDir) childrenForest
    where
        callSelf = fileTreeGenerator getDirectoryContents doesDirectoryExist
        removeBannedDirectories = filter (`notElem` bannedDirectories)
        getFilteredDirectoryContents = do
            contents <- getDirectoryContents root
            return (removeBannedDirectories contents)
        bannedDirectories = [".", "..", ".git"]
