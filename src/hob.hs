module Main where

import Control.Monad       (forM)
import Control.Monad.Trans (liftIO)
import Data.Tree
import Graphics.UI.Gtk
import Hob.Ui
import System.Directory
import System.FilePath

main :: IO ()
main = do
        projectRoot <- getCurrentDirectory
        mainWindow <- loadGui (fileTreeFromDirectory projectRoot) readFile
        _ <- mainWindow `on` deleteEvent $ liftIO mainQuit >> return False
        widgetShowAll mainWindow
        mainGUI


fileTreeFromDirectory :: FilePath -> IO (Forest DirectoryTreeElement)
fileTreeFromDirectory path = do
    directoryContents <- getFilteredDirectoryContents
    forM directoryContents $ \ child -> do
        let childPath = path </> child
        isDir <- doesDirectoryExist childPath
        childrenForest <- if isDir then fileTreeFromDirectory childPath else return []
        return $ Node (DirectoryTreeElement child childPath) childrenForest
    where
        removeDotDirectories = filter (\child -> not ((child == ".") || (child == "..")))
        getFilteredDirectoryContents = do
            contents <- getDirectoryContents path
            return (removeDotDirectories contents)
