module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.General.CssProvider
import Graphics.UI.Gtk.Builder
import Control.Monad.Trans(liftIO)
import Control.Monad (liftM, forM, filterM)
import Data.Tree
import Graphics.UI.Gtk.ModelView as Mv
import Graphics.UI.Gtk.General.StyleContext
import System.Directory
import System.FilePath

-- import Graphics.UI.Gtk.ModelView.TreeStore

fileTreeFromDirectory :: FilePath -> IO (Forest String)
fileTreeFromDirectory path = do
    contents <- getDirectoryContents path
    forM (removeDotDirectories contents) $ \ child -> do
        let childPath = path </> child
        isDir <- doesDirectoryExist childPath
        childrenForest <- if isDir then fileTreeFromDirectory childPath else return []
        return $ Node child childrenForest
    where
        removeDotDirectories = filter (\child -> not ((child == ".") || (child == "..")))

setGtkStyle = do
    --ctx <- widgetGetStyleContext mainWindow
    cssProvider <- cssProviderNew
    cssProviderLoadFromPath cssProvider ("ui" </> "themes" </> "adwaitaGrnPlus" </> "gtk-dark.css")
    --styleContextAddProvider ctx cssProvider 800
    maybe (return()) (\screen -> styleContextAddProviderForScreen screen cssProvider 800) =<< screenGetDefault


initSideBarFileTree treeView = do
    treeModel <- treeStoreNew =<< fileTreeFromDirectory =<< getCurrentDirectory
    customStoreSetColumn treeModel (makeColumnIdString 0) id

    col <- treeViewColumnNew

    rend <- Mv.cellRendererTextNew
    Mv.cellLayoutPackStart col rend True
    Mv.cellLayoutSetAttributes col rend treeModel (\v -> [Mv.cellText := v])

    treeViewAppendColumn treeView col

    treeViewSetHeadersVisible treeView False
    treeViewSetModel treeView treeModel

    treeViewSetSearchColumn treeView $ makeColumnIdString 0

    treeView `on` rowCollapsed $ \ _ _ -> treeViewColumnsAutosize treeView


main = do
    initGUI

    builder <- builderNew
    builderAddFromFile builder "ui/ui.glade"

    initSideBarFileTree =<< builderGetObject builder castToTreeView "directoryListing"

    setGtkStyle

    mainWindow <- builderGetObject builder castToWindow "mainWindow"
    mainWindow `on` deleteEvent $ liftIO mainQuit >> return False
    widgetShowAll mainWindow
    mainGUI
