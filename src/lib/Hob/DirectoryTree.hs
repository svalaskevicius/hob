module Hob.DirectoryTree (
    DirectoryForest,
    DirectoryTreeLoader,
    DirectoryReader,
    IsDirectoryCheck,
    DirectoryTreeElement(..),
    fileTreeGenerator
) where

import           Control.Monad   (forM)
import           Data.List       (sortBy)
import           Data.Tree
import           System.FilePath

type IsDirectory = Bool

data DirectoryTreeElement = DirectoryTreeElement { elementLabel :: String, elementPath :: FilePath, isDirectory :: IsDirectory }

instance Show (DirectoryTreeElement) where
  show (DirectoryTreeElement label path isDir) = show label ++" ["++ show path ++"] "++(if isDir then "(directory)" else "(file)")

instance  Eq (DirectoryTreeElement)  where
    (DirectoryTreeElement label path isDir) == (DirectoryTreeElement label' path' isDir') = (label == label') && (path == path') && (isDir == isDir')

type DirectoryForest = Forest DirectoryTreeElement
type DirectoryTreeLoader = FilePath -> IO DirectoryForest
type DirectoryReader = FilePath -> IO [FilePath]
type IsDirectoryCheck = FilePath -> IO Bool

fileTreeGenerator :: DirectoryReader -> IsDirectoryCheck -> DirectoryTreeLoader
fileTreeGenerator getDirectoryContents doesDirectoryExist root = do
    directoryContents <- retrieveDirectoryContents
    forM directoryContents $ \ (name, path, isDir) -> do
        childrenForest <- if isDir then callSelf path else return []
        return $ Node (DirectoryTreeElement name path isDir) childrenForest
    where
        callSelf = fileTreeGenerator getDirectoryContents doesDirectoryExist
        removeBannedDirectories = filter (`notElem` bannedDirectories)
        addFileInfo contents =
            forM contents $ \ child -> do
                let childPath = root </> child
                isDir <- doesDirectoryExist childPath
                return (child, childPath, isDir)
        retrieveDirectoryContents = do
            contents <- getDirectoryContents root
            let filteredContents = removeBannedDirectories contents
            filteredFileInfo <- addFileInfo filteredContents
            return $ sortBy contentsSortCriteria filteredFileInfo
        bannedDirectories = [".", "..", ".git"]
        contentsSortCriteria (name1, _, isDir1) (name2, _, isDir2)
            | isDir1 && not isDir2 = LT
            | isDir2 && not isDir1 = GT
            | otherwise = compare name1 name2

