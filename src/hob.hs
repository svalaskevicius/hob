module Main where

import Control.Monad                        (forM)
import Control.Monad.Trans                  (liftIO)
import Data.Text                            (unpack)
import Data.Tree
import Graphics.UI.Gtk
import Graphics.UI.Gtk.General.CssProvider
import Graphics.UI.Gtk.General.StyleContext
import Graphics.UI.Gtk.ModelView            as Mv
import Graphics.UI.Gtk.SourceView           (sourceBufferNewWithLanguage,
                                             sourceBufferSetHighlightSyntax,
                                             sourceBufferSetStyleScheme,
                                             sourceLanguageManagerGetLanguage,
                                             sourceLanguageManagerGetSearchPath,
                                             sourceLanguageManagerNew,
                                             sourceStyleSchemeManagerGetDefault,
                                             sourceStyleSchemeManagerGetScheme,
                                             sourceViewNewWithBuffer,
                                             sourceViewSetShowLineNumbers)
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

        builder <- loadUiBuilder
        initSidebar builder
        initMainWindow builder

        setGtkStyle
        mainGUI
    where
        loadUiBuilder = do
            builder <- builderNew
            builderAddFromFile builder "ui/ui.glade"
            return builder
        initSidebar builder = do
            sidebarTree <- builderGetObject builder castToTreeView "directoryListing"
            mainEditNotebook <- builderGetObject builder castToNotebook "tabbedEditArea"
            initSideBarFileTree sidebarTree $ launchNewFileEditor mainEditNotebook
        initMainWindow builder = do
            mainWindow <- builderGetObject builder castToWindow "mainWindow"
            _ <- mainWindow `on` deleteEvent $ liftIO mainQuit >> return False
            widgetShowAll mainWindow


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

setGtkStyle :: IO ()
setGtkStyle = do
    -- should we also listen for `screenChanged`?
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
launchNewFileEditor targetNotebook filePath = do
    lm <- sourceLanguageManagerNew
    langM <- sourceLanguageManagerGetLanguage lm "haskell"
    lang <- case langM of
        (Just lang) -> return lang
        Nothing -> do
            langDirs <- sourceLanguageManagerGetSearchPath lm
            error ("please copy haskell.lang to one of the following directories:\n"
                ++unlines langDirs)

    styleManager <- sourceStyleSchemeManagerGetDefault
    style <- sourceStyleSchemeManagerGetScheme styleManager "oblivion"

    buffer <- sourceBufferNewWithLanguage lang
    fileContents <- readFile filePath
    textBufferSetText buffer fileContents
    textBufferSetModified buffer False
    sourceBufferSetHighlightSyntax buffer True
    sourceBufferSetStyleScheme buffer (Just style)

    editor <- sourceViewNewWithBuffer buffer
    sourceViewSetShowLineNumbers editor True

    scrolledWindow <- scrolledWindowNew Nothing Nothing
    scrolledWindow `containerAdd` editor

    font <- fontDescriptionFromString "monospace 12"
    widgetModifyFont editor (Just font)

    _ <- editor `on` keyPressEvent $ do
        modifier <- eventModifier
        key <- eventKeyName
        case (modifier, unpack key) of
            ([Control], "s") -> liftIO $ saveFile buffer filePath
            _ -> return False

    widgetShowAll scrolledWindow
    _ <- notebookAppendPage targetNotebook scrolledWindow "t"
    notebookSetShowTabs targetNotebook True
    return()


saveFile :: TextBufferClass a => a -> FilePath -> IO Bool
saveFile buffer filePath = do
    writeFile filePath =<< buffer `get` textBufferText
    return True
