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
import System.IO (hPutStrLn, stderr)
import Data.Maybe (fromJust)


data DirectoryTreeElement = DirectoryTreeElement String FilePath

directoryTreeElementLabel :: DirectoryTreeElement -> String
directoryTreeElementLabel (DirectoryTreeElement label _) = label

directoryTreeElementPath :: DirectoryTreeElement -> FilePath
directoryTreeElementPath (DirectoryTreeElement _ path) = path

type NewFileEditorLauncher = FilePath -> IO()



main = do
    initGUI

    builder <- builderNew
    builderAddFromFile builder "ui/ui.glade"

    sidebarTree <- builderGetObject builder castToTreeView "directoryListing"
    mainEditNotebook <- builderGetObject builder castToNotebook "tabbedEditArea"
    initSideBarFileTree sidebarTree $ launchNewFileEditor mainEditNotebook

    setGtkStyle

    mainWindow <- builderGetObject builder castToWindow "mainWindow"
    mainWindow `on` deleteEvent $ liftIO mainQuit >> return False
    widgetShowAll mainWindow

    mainGUI


fileTreeFromDirectory :: FilePath -> IO (Forest DirectoryTreeElement)
fileTreeFromDirectory path = do
    contents <- getDirectoryContents path
    forM (removeDotDirectories contents) $ \ child -> do
        let childPath = path </> child
        isDir <- doesDirectoryExist childPath
        childrenForest <- if isDir then fileTreeFromDirectory childPath else return []
        return $ Node (DirectoryTreeElement child childPath) childrenForest
    where
        removeDotDirectories = filter (\child -> not ((child == ".") || (child == "..")))

setGtkStyle = do
    cssProvider <- cssProviderNew
    cssProviderLoadFromPath cssProvider ("ui" </> "themes" </> "adwaitaGrnPlus" </> "gtk-dark.css")
    maybe (return()) (\screen -> styleContextAddProviderForScreen screen cssProvider 800) =<< screenGetDefault


initSideBarFileTree :: TreeView -> NewFileEditorLauncher -> IO ()
initSideBarFileTree treeView launchFile = do
    treeModel <- treeStoreNew =<< fileTreeFromDirectory =<< getCurrentDirectory
    customStoreSetColumn treeModel (makeColumnIdString 0) directoryTreeElementLabel

    col <- treeViewColumnNew

    rend <- Mv.cellRendererTextNew
    Mv.cellLayoutPackStart col rend True
    Mv.cellLayoutSetAttributes col rend treeModel (\v -> [Mv.cellText := directoryTreeElementLabel v])

    treeViewAppendColumn treeView col

    treeViewSetHeadersVisible treeView False
    treeViewSetModel treeView treeModel

    treeViewSetSearchColumn treeView $ makeColumnIdString 0

    treeView `on` rowCollapsed $ \ _ _ -> treeViewColumnsAutosize treeView
    treeView `on` rowActivated $ \ path _ -> (launchFile . directoryTreeElementPath) =<< treeStoreGetValue treeModel path

    return ()


launchNewFileEditor :: Notebook -> NewFileEditorLauncher
launchNewFileEditor targetNotebook filepath = do
    editor <- textViewNew
    widgetShowAll editor
    notebookAppendPage targetNotebook editor "t"
    notebookSetShowTabs targetNotebook True
    return()

