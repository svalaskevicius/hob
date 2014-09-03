module Hob.Ui (loadGui,
               NewFileNameChooser,
               getActiveEditorText,
               getActiveEditorTab,
               launchNewEditorForText,
               launchNewFileEditor,
               getEditorText,
               closeCurrentEditorTab,
               editNewFile,
               saveCurrentEditorTab,
               toggleFocusOnCommandEntry,
               searchPreview,
               searchReset,
               searchExecute,
               getActiveEditor) where

import Control.Monad                        (filterM, unless, (<=<))
import Control.Monad.Trans                  (liftIO)
import Data.Maybe                           (fromJust, mapMaybe, isNothing, isJust)
import Data.Text                            (Text (..), pack, unpack)
import Data.Tree
import Filesystem.Path.CurrentOS            (decodeString, encodeString,
                                             filename)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.General.CssProvider
import qualified Graphics.UI.Gtk.General.StyleContext as GtkSc
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

import Hob.Command
import Hob.Context
import Hob.Context.FileContext
import Hob.Context.StyleContext
import Hob.DirectoryTree
import System.FilePath
import System.Glib.GObject

import Data.IORef

type NewFileEditorLauncher = FilePath -> IO ()
type NewFileNameChooser = IO (Maybe FilePath)

searchPreview' :: String -> Context -> IO ()
searchPreview' text ctx = searchPreview ctx text

searchExecute' :: String -> Context -> IO ()
searchExecute' text ctx = searchExecute ctx text

-- add command, dispatch and clear
commandPreviewPreviewState :: IO (PreviewCommand -> IO(), Context -> IO())
commandPreviewPreviewState = do
    state <- newIORef Nothing
    return (
                writeIORef state . Just,
                \ctx -> do 
                    resetCommand <- readIORef state
                    maybeDo (\cmd -> previewReset cmd $ ctx) resetCommand
                    writeIORef state Nothing 
            )


loadGui :: FileContext -> StyleContext -> IO Context
loadGui fileContext styleContext = do
        _ <- initGUI

        builder <- loadUiBuilder
        setGtkStyle styleContext
        let commands = [
                           (([Control], "w"), Command Nothing closeCurrentEditorTab),
                           (([Control], "s"), Command Nothing (runWith saveCurrentEditorTab fileChooser)),
                           (([Control], "n"), Command Nothing editNewFile),
                           (([], "Escape"), Command Nothing toggleFocusOnCommandEntry)
                       ]
        let cmdMatcher = CommandMatcher {
            matchKeyBinding = findCommandByShortCut commands,
            matchCommand = (\text -> (case text of
                                         '/':searchText -> Just $ Command (Just $ PreviewCommand (searchPreview' searchText) searchReset) (searchExecute' searchText);
                                          _ -> Nothing))
        }

        ctx <- initMainWindow builder cmdMatcher
        initSidebar ctx builder
        initCommandEntry ctx builder cmdMatcher
        return ctx
    where
        loadUiBuilder = do
            builder <- builderNew
            builderAddFromFile builder $ uiFile styleContext
            return builder
        initSidebar ctx builder = do
            sidebarTree <- builderGetObject builder castToTreeView "directoryListing"
            widgetSetName sidebarTree "directoryListing"
            mainEditNotebook <- builderGetObject builder castToNotebook "tabbedEditArea"
            initSideBarFileTree fileContext sidebarTree $ launchNewFileEditor ctx mainEditNotebook
        initCommandEntry ctx builder cmdMatcher = do
            commandEntry <- builderGetObject builder castToEntry "command"
            widgetSetName commandEntry "commandEntry"
            styleContext <- widgetGetStyleContext commandEntry
            mainWindow <- builderGetObject builder castToWindow "mainWindow"
            (setLastPreviewCmd, dispatchLastPreviewReset) <- commandPreviewPreviewState
            commandEntry `on` editableChanged $ do
                text <- entryGetText commandEntry
                dispatchLastPreviewReset ctx
                if text == "" then do
                    GtkSc.styleContextRemoveClass styleContext "error"
                else do
                    let command = matchCommand cmdMatcher text
                    if isNothing command then do
                        GtkSc.styleContextAddClass styleContext "error"
                    else do
                        GtkSc.styleContextRemoveClass styleContext "error"
                        let prev = commandPreview $ fromJust command
                        if isJust prev then do
                            setLastPreviewCmd $ fromJust prev
                            previewExecute (fromJust prev) ctx
                        else return()

            _ <- commandEntry `on` keyPressEvent $ do
                modifier <- eventModifier
                key <- eventKeyName
                case (modifier, unpack key) of
                    ([], "Return") -> liftIO $ do
                        text <- entryGetText commandEntry
                        if text == "" then do
                            GtkSc.styleContextRemoveClass styleContext "error"
                        else do
                            let command = matchCommand cmdMatcher text
                            if isNothing command then do
                                GtkSc.styleContextAddClass styleContext "error"
                            else do
                                GtkSc.styleContextRemoveClass styleContext "error"
                                commandExecute (fromJust command) ctx

                        return True
                    _ -> return False


            return ()
        initMainWindow builder cmdMatcher = do
            mainWindow <- builderGetObject builder castToWindow "mainWindow"
            mainNotebook <- builderGetObject builder castToNotebook "tabbedEditArea"
            commandEntry <- builderGetObject builder castToEntry "command"
            let ctx = Context styleContext fileContext mainWindow mainNotebook commandEntry
            widgetSetName mainWindow "mainWindow"
            _ <- mainWindow `on` keyPressEvent $ do
                modifier <- eventModifier
                key <- eventKeyName
                maybe (return False)
                      (\cmd -> liftIO $ commandExecute cmd ctx >> return True) $ 
                      matchKeyBinding cmdMatcher (modifier, unpack key)
            return ctx
        findCommandByShortCut [] shortCut = Nothing
        findCommandByShortCut ((s, cmd):xs) shortCut = if s == shortCut then Just cmd else findCommandByShortCut xs shortCut
        fileChooser ctx = do
            dialog <- fileChooserDialogNew Nothing (Just $ mainWindow ctx) FileChooserActionSave [("Cancel", ResponseCancel), ("Save", ResponseOk)]
            response <- dialogRun dialog
            file <- if response == ResponseOk then fileChooserGetFilename dialog else return Nothing
            widgetDestroy dialog
            return file
        runWith a b ctx = do
            a (b ctx) ctx

setGtkStyle :: StyleContext -> IO ()
setGtkStyle styleContext = do
    cssProvider <- cssProviderNew
    cssProviderLoadFromPath cssProvider $ uiTheme styleContext
    maybe (return()) (\screen -> GtkSc.styleContextAddProviderForScreen screen cssProvider 800) =<< screenGetDefault


initSideBarFileTree :: FileContext -> TreeView -> NewFileEditorLauncher -> IO ()
initSideBarFileTree fileCtx treeView launchFile = do
    let fileTreeLoader = contextFileTreeLoader fileCtx
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


launchNewFileEditor :: Context -> Notebook -> NewFileEditorLauncher
launchNewFileEditor ctx targetNotebook filePath = do
    let fileLoader = contextFileLoader . fileContext $ ctx
    editors <- mapM getEditorFromNotebookTab <=< containerGetChildren $ targetNotebook
    editorsForFile <- filterM (\(_, ed) -> isEditorFileMatching ed ) $ numberedJusts editors
    case alreadyLoadedPage editorsForFile of
        Just nr -> notebookSetCurrentPage targetNotebook nr
        Nothing -> maybeDo launchEditor =<< fileLoader filePath

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
    maybeDo (setBufferLanguage buffer <=< sourceLanguage (fileContext ctx)) filePath

    sourceBufferBeginNotUndoableAction buffer
    textBufferSetText buffer text
    textBufferSetModified buffer False
    sourceBufferEndNotUndoableAction buffer

    sourceBufferSetStyleScheme buffer =<< sourceStyleScheme (styleContext ctx) filePath

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

    widgetModifyFont editor =<< sourceStyleFont (styleContext ctx) filePath

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


closeCurrentEditorTab :: Context -> IO ()
closeCurrentEditorTab ctx = do
    currentPage <- notebookGetCurrentPage tabbed
    nthPage <- notebookGetNthPage tabbed currentPage
    case nthPage of
        Just pageContents -> do
            notebookRemovePage tabbed currentPage
            widgetDestroy pageContents
        Nothing -> return ()
    where tabbed = mainNotebook ctx

editNewFile :: Context -> IO ()
editNewFile ctx = do
    _ <- launchNewEditorForText ctx tabbed Nothing $ pack ""
    return ()
    where tabbed = mainNotebook ctx

saveCurrentEditorTab :: NewFileNameChooser -> Context -> IO ()
saveCurrentEditorTab newFileNameChooser ctx =
    maybeDo saveEditor =<< getActiveEditor ctx
    where fileWriter = contextFileWriter . fileContext $ ctx
          saveEditor editor = do
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

toggleFocusOnCommandEntry :: Context -> IO ()
toggleFocusOnCommandEntry ctx = do
    isFocused <- widgetGetIsFocus cmdEntry
    if isFocused then
        maybeDo widgetGrabFocus =<< getActiveEditor ctx
    else
        widgetGrabFocus cmdEntry
    where cmdEntry = commandEntry ctx

searchPreview :: Context -> String -> IO ()
searchPreview ctx text =
    maybeDo updateSearchPreview =<< getActiveEditor ctx
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

searchReset :: Context -> IO ()
searchReset ctx =
    maybeDo resetSearchPreview =<< getActiveEditor ctx
    where
        resetSearchPreview editor = do
            buffer <- textViewGetBuffer editor
            tagTable <- textBufferGetTagTable buffer
            maybeDo (removeEditorTag buffer) =<< textTagTableLookup tagTable "search"
        removeEditorTag buffer tag = do
            (start, end) <- textBufferGetBounds buffer
            textBufferRemoveTag buffer tag start end

searchExecute :: Context -> String -> IO ()
searchExecute ctx text =
    maybeDo doSearch =<< getActiveEditor ctx
    where
        doSearch editor = do
            buffer <- textViewGetBuffer editor
            (_, start) <- textBufferGetSelectionBounds buffer
            maybe (retryFromStart editor buffer) (selectMatch editor buffer) =<< findNextResult start
        findNextResult start = textIterForwardSearch start text [TextSearchTextOnly] Nothing
        selectMatch editor buffer (start, end) = do
            textBufferSelectRange buffer start end
            caretMark <- textBufferGetInsert buffer
            textViewScrollToMark editor caretMark 0.1 Nothing
        retryFromStart editor buffer = do
            (start, _) <- textBufferGetBounds buffer
            maybeDo (selectMatch editor buffer) =<< findNextResult start

fileNameQuark :: IO Quark
fileNameQuark = quarkFromString "fileName"

getActiveEditorText :: Context -> IO (Maybe Text)
getActiveEditorText ctx = do
    editor <- getActiveEditor ctx
    maybe (return Nothing) ((return . Just) <=< getEditorText) editor

getEditorText :: TextViewClass a => a -> IO Text
getEditorText textEdit = do
    textBuf <- textViewGetBuffer textEdit
    get textBuf textBufferText

getActiveEditor :: Context -> IO (Maybe SourceView)
getActiveEditor = maybe (return Nothing) getEditorFromNotebookTab <=< getActiveEditorTab

getEditorFromNotebookTab :: Widget -> IO (Maybe SourceView)
getEditorFromNotebookTab currentlyActiveEditor =
    if currentlyActiveEditor `isA` gTypeScrolledWindow then do
        let textEditScroller = castToScrolledWindow currentlyActiveEditor
        textEdit <- binGetChild textEditScroller
        return $ fmap castToSourceView textEdit
    else return Nothing

getActiveEditorTab :: Context -> IO (Maybe Widget)
getActiveEditorTab ctx = do
    pageNum <- notebookGetCurrentPage tabbed
    if pageNum < 0 then
        return Nothing
    else do
        tabs <- containerGetChildren tabbed
        return $ Just $ tabs!!pageNum
    where tabbed = mainNotebook ctx

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
