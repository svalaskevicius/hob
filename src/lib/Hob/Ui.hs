module Hob.Ui where

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
                                             sourceStyleSchemeManagerGetScheme, sourceStyleSchemeManagerSetSearchPath,
                                             sourceViewNewWithBuffer,
                                             sourceViewSetShowLineNumbers)
import System.FilePath

data DirectoryTreeElement = DirectoryTreeElement String FilePath

directoryTreeElementLabel :: DirectoryTreeElement -> String
directoryTreeElementLabel (DirectoryTreeElement label _) = label

directoryTreeElementPath :: DirectoryTreeElement -> FilePath
directoryTreeElementPath (DirectoryTreeElement _ path) = path

type FileTreeLoader = IO (Forest DirectoryTreeElement)
type NewFileEditorLauncher = FilePath -> IO()

type FileLoader = FilePath -> IO String

loadGui :: FileTreeLoader -> FileLoader -> IO Window
loadGui fileTreeLoader fileLoader = do
        _ <- initGUI

        builder <- loadUiBuilder
        setGtkStyle
        initSidebar builder
        initMainWindow builder
    where
        loadUiBuilder = do
            builder <- builderNew
            builderAddFromFile builder "ui/ui.glade"
            return builder
        initSidebar builder = do
            sidebarTree <- builderGetObject builder castToTreeView "directoryListing"
            widgetSetName sidebarTree "directoryListing"
            mainEditNotebook <- builderGetObject builder castToNotebook "tabbedEditArea"
            initSideBarFileTree sidebarTree fileTreeLoader $ launchNewFileEditor fileLoader mainEditNotebook
        initMainWindow builder = do
            mainWindow <- builderGetObject builder castToWindow "mainWindow"
            widgetSetName mainWindow "mainWindow"
            return mainWindow


setGtkStyle :: IO ()
setGtkStyle = do
    cssProvider <- cssProviderNew
    cssProviderLoadFromPath cssProvider ("ui" </> "themes" </> "gtk" </> "default" </> "gtk-dark.css")
    maybe (return()) (\screen -> styleContextAddProviderForScreen screen cssProvider 800) =<< screenGetDefault


initSideBarFileTree :: TreeView -> FileTreeLoader -> NewFileEditorLauncher -> IO ()
initSideBarFileTree treeView fileTreeLoader launchFile = do
    treeModel <- treeStoreNew =<< fileTreeLoader
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


launchNewFileEditor :: FileLoader -> Notebook -> NewFileEditorLauncher
launchNewFileEditor loadFile targetNotebook filePath = do
    lm <- sourceLanguageManagerNew
    langM <- sourceLanguageManagerGetLanguage lm "haskell"
    lang <- case langM of
        (Just lang) -> return lang
        Nothing -> do
            langDirs <- sourceLanguageManagerGetSearchPath lm
            error ("please copy haskell.lang to one of the following directories:\n"
                ++unlines langDirs)

    styleManager <- sourceStyleSchemeManagerGetDefault
    sourceStyleSchemeManagerSetSearchPath styleManager (Just ["ui" </> "themes" </> "gtksourceview"])
    style <- sourceStyleSchemeManagerGetScheme styleManager "molokai"

    buffer <- sourceBufferNewWithLanguage lang
    fileContents <- loadFile filePath
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
    tabNr <- notebookAppendPage targetNotebook scrolledWindow "t"
    notebookSetCurrentPage targetNotebook tabNr
    notebookSetShowTabs targetNotebook True
    return()


saveFile :: TextBufferClass a => a -> FilePath -> IO Bool
saveFile buffer filePath = do
    writeFile filePath =<< buffer `get` textBufferText
    return True
