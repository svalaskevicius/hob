module Hob.Ui where

import Control.Monad                        (unless, (<=<))
import Control.Monad.Trans                  (liftIO)
import Data.Maybe                           (fromJust)
import Data.Text                            (Text (..), pack, unpack)
import Data.Tree
import Filesystem.Path.CurrentOS            (decodeString, encodeString,
                                             filename)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.General.CssProvider
import Graphics.UI.Gtk.General.StyleContext
import Graphics.UI.Gtk.ModelView            as Mv
import Graphics.UI.Gtk.SourceView           (SourceView (..),
                                             sourceBufferBeginNotUndoableAction,
                                             sourceBufferEndNotUndoableAction,
                                             sourceBufferNewWithLanguage,
                                             sourceBufferSetHighlightSyntax,
                                             sourceBufferSetStyleScheme,
                                             sourceLanguageManagerGetLanguage,
                                             sourceLanguageManagerGetSearchPath,
                                             sourceLanguageManagerNew,
                                             sourceStyleSchemeManagerGetDefault,
                                             sourceStyleSchemeManagerGetScheme, sourceStyleSchemeManagerSetSearchPath,
                                             sourceViewNewWithBuffer,
                                             sourceViewSetShowLineNumbers)
import Hob.DirectoryTree
import System.FilePath
import System.Glib.GObject

type FileTreeLoader = IO (Forest DirectoryTreeElement)
type NewFileEditorLauncher = FilePath -> IO ()

type FileLoader = FilePath -> IO (Maybe Text)
type FileWriter = FilePath -> Text -> IO ()

loadGui :: FileTreeLoader -> FileLoader -> FileWriter -> IO Window
loadGui fileTreeLoader fileLoader fileWriter = do
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
            welcomeTab <- builderGetObject builder castToLabel "welcomeText"
            widgetSetName welcomeTab "welcomeText"
            _ <- mainWindow `on` keyPressEvent $ do
                modifier <- eventModifier
                key <- eventKeyName
                case (modifier, unpack key) of
                    ([Control], "w") -> liftIO $ closeCurrentEditorTab mainWindow >> return True
                    ([Control], "s") -> liftIO $ saveCurrentEditorTab fileWriter mainWindow >> return True
                    _ -> return False

            return mainWindow


setGtkStyle :: IO ()
setGtkStyle = do
    cssProvider <- cssProviderNew
    cssProviderLoadFromPath cssProvider ("ui" </> "themes" </> "gtk" </> "default" </> "gtk-dark.css")
    maybe (return()) (\screen -> styleContextAddProviderForScreen screen cssProvider 800) =<< screenGetDefault


initSideBarFileTree :: TreeView -> FileTreeLoader -> NewFileEditorLauncher -> IO ()
initSideBarFileTree treeView fileTreeLoader launchFile = do
    treeModel <- treeStoreNew =<< fileTreeLoader
    customStoreSetColumn treeModel (makeColumnIdString 0) elementLabel

    col <- treeViewColumnNew

    rend <- Mv.cellRendererTextNew
    Mv.cellLayoutPackStart col rend True
    Mv.cellLayoutSetAttributes col rend treeModel (\v -> [Mv.cellText := elementLabel v])

    _ <- treeViewAppendColumn treeView col

    treeViewSetHeadersVisible treeView False
    treeViewSetModel treeView treeModel

    treeViewSetSearchColumn treeView searchCol


    _ <- treeView `on` rowCollapsed $ \ _ _ -> treeViewColumnsAutosize treeView
    _ <- treeView `on` rowActivated $ \ path _ -> activateRow =<< treeStoreGetValue treeModel path

    return ()

    where
        searchCol :: ColumnId row String
        searchCol = makeColumnIdString 0

        activateRow :: DirectoryTreeElement -> IO ()
        activateRow el = unless (isDirectory el) $ (launchFile . elementPath) el


launchNewFileEditor :: FileLoader -> Notebook -> NewFileEditorLauncher
launchNewFileEditor loadFile targetNotebook filePath = do
    fileContents <- loadFile filePath
    maybe (return ()) launchEditor fileContents
    where launchEditor text = do
              editor <- launchNewEditorForText targetNotebook tabTitle text
              quark <- fileNameQuark
              objectSetAttribute quark editor $ Just filePath
              return ()
          tabTitle = filename' filePath
          filename' = encodeString . filename . decodeString


launchNewEditorForText :: Notebook -> String -> Text -> IO SourceView
launchNewEditorForText targetNotebook title text = do
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
    sourceBufferBeginNotUndoableAction buffer
    textBufferSetText buffer text
    textBufferSetModified buffer False
    sourceBufferEndNotUndoableAction buffer

    sourceBufferSetHighlightSyntax buffer True
    sourceBufferSetStyleScheme buffer (Just style)

    editor <- sourceViewNewWithBuffer buffer
    sourceViewSetShowLineNumbers editor True

    scrolledWindow <- scrolledWindowNew Nothing Nothing
    scrolledWindow `containerAdd` editor

    font <- fontDescriptionFromString "monospace 12"
    widgetModifyFont editor (Just font)

    widgetShowAll scrolledWindow
    tabNr <- notebookAppendPage targetNotebook scrolledWindow title
    notebookSetCurrentPage targetNotebook tabNr
    notebookSetShowTabs targetNotebook True
    return editor


closeCurrentEditorTab :: Window -> IO ()
closeCurrentEditorTab mainWindow = do
    tabbed <- getActiveEditorNotebook mainWindow
    currentPage <- notebookGetCurrentPage tabbed
    nthPage <- notebookGetNthPage tabbed currentPage
    case nthPage of
        Just pageContents -> do
            notebookRemovePage tabbed currentPage
            widgetDestroy pageContents
        Nothing -> return ()

saveCurrentEditorTab :: FileWriter -> Window -> IO ()
saveCurrentEditorTab fileWriter mainWindow = do
    editor <- getActiveEditor mainWindow
    maybe (return ()) saveEditor editor
    where saveEditor editor = do
              quark <- fileNameQuark
              path <- objectGetAttributeUnsafe quark editor
              case path of
                  Just filePath -> do
                      text <- getEditorText editor
                      fileWriter filePath text
                      return ()
                  Nothing -> return ()

fileNameQuark :: IO Quark
fileNameQuark = quarkFromString "fileName"

getActiveEditorText :: Window -> IO (Maybe Text)
getActiveEditorText mainWindow = do
    editor <- getActiveEditor mainWindow
    maybe (return Nothing) ((return . Just) <=< getEditorText) editor

getEditorText :: TextViewClass a => a -> IO Text
getEditorText textEdit = do
      textBuf <- textViewGetBuffer textEdit
      get textBuf textBufferText

getActiveEditor :: Window -> IO (Maybe TextView)
getActiveEditor mainWindow = do
      currentlyActiveEditor <- getActiveEditorTab mainWindow
      if currentlyActiveEditor `isA` gTypeScrolledWindow then do
          let textEditScroller = castToScrolledWindow currentlyActiveEditor
          textEdit' <- binGetChild textEditScroller
          return $ fmap castToTextView textEdit'
      else return Nothing

getActiveEditorTab :: Window -> IO Widget
getActiveEditorTab mainWindow = do
      tabbed <- getActiveEditorNotebook mainWindow
      pageNum <- notebookGetCurrentPage tabbed
      tabs <- containerGetChildren tabbed
      return (tabs!!pageNum)

getActiveEditorNotebook :: Window -> IO Notebook
getActiveEditorNotebook mainWindow = do
      paned <- binGetChild mainWindow
      tabbed' <- panedGetChild2 $ castToPaned $ fromJust paned
      return $ castToNotebook $ fromJust tabbed'
