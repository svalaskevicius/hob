module Main where

import Control.Monad       (forM)
import Control.Monad.Trans (liftIO)
import Data.Tree
import Graphics.UI.Gtk
import Hob.DirectoryTree
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
fileTreeFromDirectory = fileTreeGenerator getDirectoryContents doesDirectoryExist
