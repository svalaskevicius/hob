module Hob.Ui (loadGui,
               FileLoader,
               FileWriter,
               NewFileNameChooser,
               getActiveEditorText,
               getActiveEditorTab,
               getActiveEditorNotebook,
               launchNewEditorForText,
               launchNewFileEditor,
               getEditorText,
               closeCurrentEditorTab,
               editNewFile,
               saveCurrentEditorTab,
               getActiveEditor) where

import Control.Monad                        (filterM, unless, (<=<))
import Control.Monad.Trans                  (liftIO)
import Data.Maybe                           (fromJust, mapMaybe)
import Data.Text                            (Text (..), pack, unpack)
import Data.Tree
import Filesystem.Path.CurrentOS            (decodeString, encodeString,
                                             filename)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.General.CssProvider
import Graphics.UI.Gtk.General.StyleContext
import Graphics.UI.Gtk.ModelView            as Mv
import Graphics.UI.Gtk.SourceView           (SourceView (..), castToSourceView,
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
type NewFileNameChooser = IO (Maybe FilePath)

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
            _ <- mainWindow `on` keyPressEvent $ do
                modifier <- eventModifier
                key <- eventKeyName
                case (modifier, unpack key) of
                    ([Control], "w") -> liftIO $ closeCurrentEditorTab mainWindow >> return True
                    ([Control], "s") -> liftIO $ saveCurrentEditorTab (fileChooser mainWindow) fileWriter mainWindow >> return True
                    ([Control], "n") -> liftIO $ editNewFile mainWindow >> return True
                    _ -> return False

            return mainWindow
        fileChooser mainWindow = do
            dialog <- fileChooserDialogNew Nothing (Just mainWindow) FileChooserActionSave [("Cancel", ResponseCancel), ("Save", ResponseOk)]
            fileChooserGetFilename dialog

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
    editors <- mapM getEditorFromNotebookTab <=< containerGetChildren $ targetNotebook
    editorsForFile <- filterM (\(_, ed) -> isEditorFileMatching ed ) $ numberedJusts editors
    case alreadyLoadedPage editorsForFile of
        Just nr -> notebookSetCurrentPage targetNotebook nr
        Nothing -> maybe (return ()) launchEditor <=< loadFile $ filePath

    where launchEditor text = do
              editor <- launchNewEditorForText targetNotebook (Just filePath) text
              return ()
          isEditorFileMatching editor = do
              quark <- fileNameQuark
              f <- objectGetAttributeUnsafe quark editor
              return $ maybe False (filePath ==) f
          alreadyLoadedPage [(nr, _)] = Just nr
          alreadyLoadedPage _ = Nothing

launchNewEditorForText :: Notebook -> Maybe FilePath -> Text -> IO SourceView
launchNewEditorForText targetNotebook filePath text = do
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

    buffer `on` modifiedChanged $ notebookSetTabLabelText targetNotebook scrolledWindow =<< tabTitleForEditor editor

    setEditorFilePath editor filePath

    return editor
    where title = tabTitle filePath


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

editNewFile :: Window -> IO ()
editNewFile mainWindow = do
    tabbed <- getActiveEditorNotebook mainWindow
    _ <- launchNewEditorForText tabbed Nothing $ pack ""
    return ()

saveCurrentEditorTab :: NewFileNameChooser -> FileWriter -> Window -> IO ()
saveCurrentEditorTab newFileNameChooser fileWriter mainWindow = do
    editor <- getActiveEditor mainWindow
    maybe (return ()) saveEditor editor
    where saveEditor editor = do
              quark <- fileNameQuark
              path <- objectGetAttributeUnsafe quark editor
              case path of
                  Just filePath -> saveEditorContents editor filePath
                  Nothing -> askForFile $ saveAsNewFile editor
          askForFile onSuccess = newFileNameChooser >>= maybe (return()) onSuccess
          saveAsNewFile editor filePath = do
              saveEditorContents editor filePath
              setEditorFilePath editor $ Just filePath
              updateEditorTitle editor

          saveEditorContents editor filePath = do
              textBuf <- textViewGetBuffer editor
              text <- get textBuf textBufferText
              fileWriter filePath text
              textBuf `set` [textBufferModified := False]
              return ()

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

getActiveEditor :: Window -> IO (Maybe SourceView)
getActiveEditor = maybe (return Nothing) getEditorFromNotebookTab <=< getActiveEditorTab

getEditorFromNotebookTab :: Widget -> IO (Maybe SourceView)
getEditorFromNotebookTab currentlyActiveEditor =
      if currentlyActiveEditor `isA` gTypeScrolledWindow then do
          let textEditScroller = castToScrolledWindow currentlyActiveEditor
          textEdit <- binGetChild textEditScroller
          return $ fmap castToSourceView textEdit
      else return Nothing

getActiveEditorTab :: Window -> IO (Maybe Widget)
getActiveEditorTab mainWindow = do
      tabbed <- getActiveEditorNotebook mainWindow
      pageNum <- notebookGetCurrentPage tabbed
      if pageNum < 0 then
          return Nothing
      else do
          tabs <- containerGetChildren tabbed
          return $ Just $ tabs!!pageNum

getActiveEditorNotebook :: Window -> IO Notebook
getActiveEditorNotebook mainWindow = do
      paned <- binGetChild mainWindow
      notebook <- panedGetChild2 $ castToPaned $ fromJust paned
      return $ castToNotebook $ fromJust notebook

tabTitle :: Maybe FilePath -> String
tabTitle (Just filePath) = filename' filePath
    where filename' = encodeString . filename . decodeString
tabTitle Nothing = "(new file)"

tabTitleForEditor :: SourceView -> IO String
tabTitleForEditor editor = do
    quark <- fileNameQuark
    filePath <- objectGetAttributeUnsafe quark editor
    buffer <- textViewGetBuffer editor
    modified <- buffer `get` textBufferModified
    return $ if modified then tabTitle filePath ++ "*" else tabTitle filePath

updateEditorTitle :: SourceView -> IO ()
updateEditorTitle editor = do
    Just scrolledW <- widgetGetParent editor
    Just notebookW <- widgetGetParent scrolledW
    let notebook = castToNotebook notebookW
    notebookSetTabLabelText notebook scrolledW =<< tabTitleForEditor editor

setEditorFilePath :: SourceView -> Maybe FilePath -> IO ()
setEditorFilePath editor filePath = do
    quark <- fileNameQuark
    objectSetAttribute quark editor filePath

liftTupledMaybe :: (a, Maybe b) -> Maybe (a, b)
liftTupledMaybe (x, Just y) = Just (x, y)
liftTupledMaybe (x, Nothing) = Nothing

numberedJusts :: [Maybe a] -> [(Int, a)]
numberedJusts a = mapMaybe liftTupledMaybe $ zip [0..] a
