module Main where

import Control.Monad                        (forM)
import Control.Monad.Trans                  (liftIO)
import Data.Tree
import Graphics.UI.Gtk
import Graphics.UI.Gtk.General.CssProvider
import Graphics.UI.Gtk.General.StyleContext
import Graphics.UI.Gtk.ModelView            as Mv
import System.Directory
import System.FilePath


data DirectoryTreeElement = DirectoryTreeElement String FilePath

directoryTreeElementLabel :: DirectoryTreeElement -> String
directoryTreeElementLabel (DirectoryTreeElement label _) = label

directoryTreeElementPath :: DirectoryTreeElement -> FilePath
directoryTreeElementPath (DirectoryTreeElement _ path) = path

type NewFileEditorLauncher = FilePath -> IO()


main :: IO ()
main = do
    _ <- initGUI

    builder <- builderNew
    builderAddFromFile builder "ui/ui.glade"

    sidebarTree <- builderGetObject builder castToTreeView "directoryListing"
    mainEditNotebook <- builderGetObject builder castToNotebook "tabbedEditArea"
    initSideBarFileTree sidebarTree $ launchNewFileEditor mainEditNotebook

    setGtkStyle

    mainWindow <- builderGetObject builder castToWindow "mainWindow"
    _ <- mainWindow `on` deleteEvent $ liftIO mainQuit >> return False
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

setGtkStyle :: IO ()
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

    _ <- treeViewAppendColumn treeView col

    treeViewSetHeadersVisible treeView False
    treeViewSetModel treeView treeModel

    treeViewSetSearchColumn treeView searchCol


    _ <- treeView `on` rowCollapsed $ \ _ _ -> treeViewColumnsAutosize treeView
    _ <- treeView `on` rowActivated $ \ path _ -> (launchFile . directoryTreeElementPath) =<< treeStoreGetValue treeModel path

    return ()

    where
        searchCol :: ColumnId row String
        searchCol = makeColumnIdString 0


launchNewFileEditor :: Notebook -> NewFileEditorLauncher
launchNewFileEditor targetNotebook _ = do
    editor <- textViewNew
    widgetShowAll editor
    _ <- notebookAppendPage targetNotebook editor "t"
    notebookSetShowTabs targetNotebook True
    return()

