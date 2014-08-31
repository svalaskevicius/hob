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
               focusCommandEntry,
               getCommandEntry,
               searchPreview,
               searchReset,
               searchExecute,
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
import Graphics.UI.Gtk.SourceView           (SourceDrawSpacesFlags (..),
                                             SourceLanguageManager, SourceView,
                                             castToSourceView,
                                             sourceBufferBeginNotUndoableAction,
                                             sourceBufferEndNotUndoableAction,
                                             sourceBufferNew,
                                             sourceBufferSetHighlightSyntax,
                                             sourceBufferSetLanguage,
                                             sourceBufferSetStyleScheme,
                                             sourceLanguageManagerGetLanguage,
                                             sourceLanguageManagerGetSearchPath,
                                             sourceLanguageManagerGuessLanguage,
                                             sourceLanguageManagerNew,
                                             sourceStyleSchemeManagerGetDefault,
                                             sourceStyleSchemeManagerGetScheme, sourceStyleSchemeManagerSetSearchPath,
                                             sourceViewNewWithBuffer,
                                             sourceViewSetAutoIndent,
                                             sourceViewSetDrawSpaces,
                                             sourceViewSetHighlightCurrentLine,
                                             sourceViewSetIndentOnTab,
                                             sourceViewSetIndentWidth, sourceViewSetInsertSpacesInsteadOfTabs,
                                             sourceViewSetShowLineNumbers,
                                             sourceViewSetTabWidth)
import Hob.Context
import Hob.DirectoryTree
import System.FilePath
import System.Glib.GObject

type FileTreeLoader = IO (Forest DirectoryTreeElement)
type NewFileEditorLauncher = FilePath -> IO ()
type NewFileNameChooser = IO (Maybe FilePath)

type FileLoader = FilePath -> IO (Maybe Text)
type FileWriter = FilePath -> Text -> IO ()

loadGui :: Context -> FileTreeLoader -> FileLoader -> FileWriter -> IO Window
loadGui ctx fileTreeLoader fileLoader fileWriter = do
        _ <- initGUI

        builder <- loadUiBuilder
        setGtkStyle ctx

        initSidebar builder
        initCommandEntry builder
        initMainWindow builder
    where
        loadUiBuilder = do
            builder <- builderNew
            builderAddFromFile builder $ uiFile ctx
            return builder
        initSidebar builder = do
            sidebarTree <- builderGetObject builder castToTreeView "directoryListing"
            widgetSetName sidebarTree "directoryListing"
            mainEditNotebook <- builderGetObject builder castToNotebook "tabbedEditArea"
            initSideBarFileTree sidebarTree fileTreeLoader $ launchNewFileEditor ctx fileLoader mainEditNotebook
        initCommandEntry builder = do
            commandEntry <- builderGetObject builder castToEntry "command"
            widgetSetName commandEntry "commandEntry"
            styleContext <- widgetGetStyleContext commandEntry
            mainWindow <- builderGetObject builder castToWindow "mainWindow"
            commandEntry `on` editableChanged $ do
                text <- entryGetText commandEntry
                case text of
                    '/':searchText -> do
                        styleContextRemoveClass styleContext "error"
                        putStrLn $ "searching for " ++ searchText
                        searchPreview mainWindow searchText
                    "" -> searchReset mainWindow >> styleContextRemoveClass styleContext "error"
                    _ -> searchReset mainWindow >> styleContextAddClass styleContext "error"


            _ <- commandEntry `on` keyPressEvent $ do
                modifier <- eventModifier
                key <- eventKeyName
                case (modifier, unpack key) of
                    ([], "Return") -> liftIO $ do
                        text <- entryGetText commandEntry
                        case text of
                            '/':searchText -> searchExecute mainWindow searchText
                            "" -> searchReset mainWindow >> styleContextRemoveClass styleContext "error"
                            _ -> searchReset mainWindow >> styleContextAddClass styleContext "error"

                        return True
                    _ -> return False


            return ()
        initMainWindow builder = do
            mainWindow <- builderGetObject builder castToWindow "mainWindow"
            widgetSetName mainWindow "mainWindow"
            _ <- mainWindow `on` keyPressEvent $ do
                modifier <- eventModifier
                key <- eventKeyName
                case (modifier, unpack key) of
                    ([Control], "w") -> liftIO $ closeCurrentEditorTab mainWindow >> return True
                    ([Control], "s") -> liftIO $ saveCurrentEditorTab (fileChooser mainWindow) fileWriter mainWindow >> return True
                    ([Control], "n") -> liftIO $ editNewFile ctx mainWindow >> return True
                    ([], "Escape") -> liftIO $ focusCommandEntry mainWindow >> return True
                    _ -> return False

            return mainWindow
        fileChooser mainWindow = do
            dialog <- fileChooserDialogNew Nothing (Just mainWindow) FileChooserActionSave [("Cancel", ResponseCancel), ("Save", ResponseOk)]
            response <- dialogRun dialog
            file <- if response == ResponseOk then fileChooserGetFilename dialog else return Nothing
            widgetDestroy dialog
            return file

setGtkStyle :: Context -> IO ()
setGtkStyle ctx = do
    cssProvider <- cssProviderNew
    cssProviderLoadFromPath cssProvider $ uiTheme ctx
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


launchNewFileEditor :: Context -> FileLoader -> Notebook -> NewFileEditorLauncher
launchNewFileEditor ctx loadFile targetNotebook filePath = do
    editors <- mapM getEditorFromNotebookTab <=< containerGetChildren $ targetNotebook
    editorsForFile <- filterM (\(_, ed) -> isEditorFileMatching ed ) $ numberedJusts editors
    case alreadyLoadedPage editorsForFile of
        Just nr -> notebookSetCurrentPage targetNotebook nr
        Nothing -> maybeDo launchEditor =<< loadFile filePath

    where launchEditor text = do
              editor <- launchNewEditorForText ctx targetNotebook (Just filePath) text
              return ()
          isEditorFileMatching editor = do
              quark <- fileNameQuark
              f <- objectGetAttributeUnsafe quark editor
              return $ maybe False (filePath ==) f
          alreadyLoadedPage [(nr, _)] = Just nr
          alreadyLoadedPage _ = Nothing

launchNewEditorForText :: Context -> Notebook -> Maybe FilePath -> Text -> IO SourceView
launchNewEditorForText ctx targetNotebook filePath text = do
    buffer <- sourceBufferNew Nothing
    maybeDo (setBufferLanguage buffer <=< sourceLanguage ctx) filePath

    sourceBufferBeginNotUndoableAction buffer
    textBufferSetText buffer text
    textBufferSetModified buffer False
    sourceBufferEndNotUndoableAction buffer

    sourceBufferSetStyleScheme buffer =<< sourceStyleScheme ctx filePath

    editor <- sourceViewNewWithBuffer buffer
    sourceViewSetShowLineNumbers editor True
    sourceViewSetAutoIndent editor True
    sourceViewSetIndentOnTab editor True
    sourceViewSetIndentWidth editor 4
    sourceViewSetTabWidth editor 4
    sourceViewSetInsertSpacesInsteadOfTabs editor True
    sourceViewSetHighlightCurrentLine editor True
    sourceViewSetDrawSpaces editor SourceDrawSpacesTrailing

    scrolledWindow <- scrolledWindowNew Nothing Nothing
    scrolledWindow `containerAdd` editor

    widgetModifyFont editor =<< sourceStyleFont ctx filePath

    widgetShowAll scrolledWindow
    tabNr <- notebookAppendPage targetNotebook scrolledWindow title
    notebookSetCurrentPage targetNotebook tabNr
    notebookSetShowTabs targetNotebook True

    buffer `on` modifiedChanged $ notebookSetTabLabelText targetNotebook scrolledWindow =<< tabTitleForEditor editor

    setEditorFilePath editor filePath

    return editor
    where
        title = tabTitle filePath
        setBufferLanguage buffer (Just lang) = sourceBufferSetLanguage buffer (Just lang) >> sourceBufferSetHighlightSyntax buffer True
        setBufferLanguage buffer Nothing = return()


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

editNewFile :: Context -> Window -> IO ()
editNewFile ctx mainWindow = do
    tabbed <- getActiveEditorNotebook mainWindow
    _ <- launchNewEditorForText ctx tabbed Nothing $ pack ""
    return ()

saveCurrentEditorTab :: NewFileNameChooser -> FileWriter -> Window -> IO ()
saveCurrentEditorTab newFileNameChooser fileWriter mainWindow = do
    maybeDo saveEditor =<< getActiveEditor mainWindow
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

focusCommandEntry  :: Window -> IO ()
focusCommandEntry mainWindow = widgetGrabFocus =<< getCommandEntry mainWindow

searchPreview :: Window -> String -> IO ()
searchPreview mainWindow text = do
    maybeDo updateSearchPreview =<< getActiveEditor mainWindow
    where
        updateSearchPreview editor = do
            buffer <- textViewGetBuffer editor
            tagTable <- textBufferGetTagTable buffer
            tag <- maybe (addNewSearchTag tagTable) return =<< textTagTableLookup tagTable "search"
            (start, end) <- textBufferGetBounds buffer
            textBufferRemoveTag buffer tag start end
            addNewSearchTags buffer tag start end
        addNewSearchTag tagTable = do
            tag <- textTagNew $ Just $ pack "search"
            tag `set` [textTagBackground := "#707550"]
            textTagTableAdd tagTable tag
            return tag
        addNewSearchTags buffer tag start end = do
            result <- textIterForwardSearch start text [TextSearchTextOnly] (Just end)
            case result of
                Just (matchStart, matchEnd) -> do
                    textBufferApplyTag buffer tag matchStart matchEnd
                    addNewSearchTags buffer tag matchEnd end
                Nothing -> return()

searchReset :: Window -> IO ()
searchReset mainWindow = do
    maybeDo resetSearchPreview =<< getActiveEditor mainWindow
    where
        resetSearchPreview editor = do
            buffer <- textViewGetBuffer editor
            tagTable <- textBufferGetTagTable buffer
            maybeDo (removeEditorTag buffer) =<< textTagTableLookup tagTable "search"
        removeEditorTag buffer tag = do
            (start, end) <- textBufferGetBounds buffer
            textBufferRemoveTag buffer tag start end

searchExecute :: Window -> String -> IO ()
searchExecute mainWindow text = do
    maybeDo doSearch =<< getActiveEditor mainWindow
    where
        doSearch editor = do
            buffer <- textViewGetBuffer editor
            (_, start) <- textBufferGetSelectionBounds buffer
            maybe (retryFromStart buffer) (selectMatch buffer) =<< findNextResult start
        findNextResult start = textIterForwardSearch start text [TextSearchTextOnly] Nothing
        selectMatch buffer (start, end) = textBufferSelectRange buffer start end
        retryFromStart buffer = do
            (start, _) <- textBufferGetBounds buffer
            maybeDo (selectMatch buffer) =<< findNextResult start

getCommandEntry :: Window -> IO Entry
getCommandEntry mainWindow = do
    children <- getMainAreaBoxChildren mainWindow
    let notebook = children !! 1
    return $ castToEntry notebook


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
    children <- getMainAreaBoxChildren mainWindow
    let notebook = head children
    return $ castToNotebook notebook

getMainAreaBoxChildren :: Window -> IO [Widget]
getMainAreaBoxChildren mainWindow = do
    paned <- binGetChild mainWindow
    box <- panedGetChild2 $ castToPaned $ fromJust paned
    containerGetChildren $ castToBox $ fromJust box

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

maybeDo :: (a -> IO ()) -> Maybe a -> IO ()
maybeDo = maybe (return())
