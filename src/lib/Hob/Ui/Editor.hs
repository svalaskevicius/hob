module Hob.Ui.Editor (
               newEditorForText,
               getActiveEditorText,
               getActiveEditorTab,
               getActiveEditor,
               invokeOnActiveEditor,
               getEditorText,
               getEditorFromNotebookTab
               ) where

import           Control.Monad.Reader
import           Data.Text                  (Text)
import           Filesystem.Path.CurrentOS  (decodeString, encodeString,
                                             filename)
import           Graphics.UI.Gtk
import           Graphics.UI.Gtk.SourceView (SourceDrawSpacesFlags (..),
                                             SourceView, castToSourceView,
                                             sourceBufferBeginNotUndoableAction,
                                             sourceBufferEndNotUndoableAction,
                                             sourceBufferNew,
                                             sourceBufferSetHighlightSyntax,
                                             sourceBufferSetLanguage,
                                             sourceBufferSetStyleScheme,
                                             sourceViewNewWithBuffer,
                                             sourceViewSetAutoIndent,
                                             sourceViewSetDrawSpaces,
                                             sourceViewSetHighlightCurrentLine,
                                             sourceViewSetIndentOnTab,
                                             sourceViewSetIndentWidth, sourceViewSetInsertSpacesInsteadOfTabs,
                                             sourceViewSetShowLineNumbers,
                                             sourceViewSetTabWidth)
import           System.Glib.GObject        (Quark)

import           Hob.Context
import           Hob.Context.FileContext
import           Hob.Context.StyleContext
import           Hob.Context.UiContext
import           Hob.Control
import qualified Hob.Ui.Editor.Search as ES

import Data.Maybe(mapMaybe)

gtkEditor :: SourceView -> Editor
gtkEditor sourceView = Editor
            { editorId = const . liftIO $ getEditorId sourceView

            , enterEditorMode = \editor mode -> do
                modes <- liftIO $ getEditorModes sourceView
                liftIO $ setEditorModes sourceView $ modes++[mode]
                return editor

            , exitLastEditorMode = \editor -> do
                modes <- liftIO $ getEditorModes sourceView
                if null modes then return editor
                else do
                    cleanup $ last modes
                    liftIO $ setEditorModes sourceView $ init modes
                    return editor

            , modeStack  = const . liftIO $ getEditorModes sourceView

            , isCurrentlyActive = const $ do
                ctx <- ask
                active <- liftIO $ getActiveEditor ctx
                return $ active == Just sourceView
            
            , getEditorFilePath = \_ -> liftIO $ getSourceViewFilePath sourceView
            
            , setEditorFilePath = \editor fp -> do
                liftIO $ setSourceViewFilePath sourceView fp
                liftIO $ updateEditorTitle sourceView
                return editor
                
            , getEditorContents = \_ -> liftIO $ do
                  textBuf <- textViewGetBuffer sourceView
                  text <- get textBuf textBufferText
                  return text
                  
            , activateEditor = \_ notebook -> liftIO $ do
                  currentEditors <- mapM getEditorFromNotebookTab <=< containerGetChildren $ notebook
                  let editorsForFile = take 1 . filter (\(_, ed) -> sourceView == ed ) . numberedJusts $ currentEditors
                  case editorsForFile of
                      [(nr, _)] ->  notebookSetCurrentPage notebook nr
                      _ -> return()
                  liftIO $ widgetGrabFocus sourceView

            , setModifiedState = \editor newState -> do
                  liftIO $ do
                      textBuf <- textViewGetBuffer sourceView
                      textBuf `set` [textBufferModified := newState]
                  return editor
                  
            , highlightSearchPreview = \editor text -> liftIO $ do
                  ES.highlightSearchPreview sourceView text
                  return editor

            , resetSearchPreview = \editor -> liftIO $ do
                  ES.resetSearchPreview sourceView
                  return editor
            
            , findFirstFromCursor = \editor text -> liftIO $ do
                  ES.findFirstFromCursor sourceView text
                  return editor

            , findNext = \editor -> liftIO $ do
                  ES.findNext sourceView
                  return editor
            
            , findPrevious = \editor -> liftIO $ do
                  ES.findPrevious sourceView
                  return editor
            
            , resetSearch = \editor -> liftIO $ do
                  ES.resetSearch sourceView
                  return editor
            
            , startReplace = \editor search replace -> liftIO $ do
                  ES.startReplace sourceView search replace
                  return editor
            
            , replaceNext = \editor -> liftIO $ do
                  ES.replaceNext sourceView
                  return editor
            
            , resetReplace = \editor -> liftIO $ do
                  ES.resetReplace sourceView
                  return editor
            
            }

numberedJusts :: [Maybe a] -> [(Int, a)]
numberedJusts a = mapMaybe liftTupledMaybe $ zip [0..] a

liftTupledMaybe :: (a, Maybe b) -> Maybe (a, b)
liftTupledMaybe (x, Just y) = Just (x, y)
liftTupledMaybe (_, Nothing) = Nothing


newEditorForText :: Notebook -> Maybe FilePath -> Text -> App ()
newEditorForText targetNotebook filePath text = do
    ctx <- ask
    liftIO $ do
        newId <- idGenerator ctx
        editor <- createNewEditor ctx
        setEditorId editor newId
        setEditorModes editor []
        updateEditors (editors ctx) $ \oldEditors -> return $ oldEditors ++ [gtkEditor editor]
    where
        title = tabTitleForFile filePath
        setBufferLanguage buffer (Just lang) = sourceBufferSetLanguage buffer (Just lang) >> sourceBufferSetHighlightSyntax buffer True
        setBufferLanguage _ Nothing = return()
        createNewEditor ctx = do
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
            sourceViewSetDrawSpaces editor [SourceDrawSpacesTrailing]

            scrolledWindow <- scrolledWindowNew Nothing Nothing
            scrolledWindow `containerAdd` editor

            widgetModifyFont editor =<< sourceStyleFont (styleContext ctx) filePath

            widgetShowAll scrolledWindow
            tabNr <- notebookAppendPage targetNotebook scrolledWindow title
            notebookSetCurrentPage targetNotebook tabNr
            notebookSetShowTabs targetNotebook True

            _ <- buffer `on` modifiedChanged $ notebookSetTabLabelText targetNotebook scrolledWindow =<< tabTitleForEditor editor

            setSourceViewFilePath editor filePath
            return editor


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

invokeOnActiveEditor :: (SourceView -> IO()) -> App ()
invokeOnActiveEditor actions = do
    ctx <- ask
    editor <- liftIO $ getActiveEditor ctx
    liftIO $ maybeDo actions editor

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
    where tabbed = mainNotebook.uiContext $ ctx

tabTitleForFile :: Maybe FilePath -> String
tabTitleForFile (Just filePath) = filename' filePath
    where filename' = encodeString . filename . decodeString
tabTitleForFile Nothing = "(new file)"

tabTitleForEditor :: SourceView -> IO String
tabTitleForEditor editor = do
    filePath <- getSourceViewFilePath editor
    buffer <- textViewGetBuffer editor
    modified <- buffer `get` textBufferModified
    return $ if modified then tabTitleForFile filePath ++ "*" else tabTitleForFile filePath

setSourceViewFilePath :: SourceView -> Maybe FilePath -> IO ()
setSourceViewFilePath editor filePath = do
    quark <- fileNameQuark
    objectSetAttribute quark editor filePath

getSourceViewFilePath :: SourceView -> IO (Maybe FilePath)
getSourceViewFilePath editor = do
    quark <- fileNameQuark
    objectGetAttributeUnsafe quark editor

setEditorId :: SourceView -> Int -> IO ()
setEditorId editor identifier = do
    quark <- editorIdQuark
    objectSetAttribute quark editor $ Just identifier

getEditorId :: SourceView -> IO Int
getEditorId editor = do
    quark <- editorIdQuark
    mRet <- objectGetAttributeUnsafe quark editor
    case mRet of
        Just ret -> return ret
        Nothing -> error "editor has no id assigned"

setEditorModes :: SourceView -> [Mode] -> IO ()
setEditorModes editor modes = do
    quark <- editorModesQuark
    objectSetAttribute quark editor $ Just modes

getEditorModes :: SourceView -> IO [Mode]
getEditorModes editor = do
    quark <- editorModesQuark
    mRet <- objectGetAttributeUnsafe quark editor
    case mRet of
        Just ret -> return ret
        Nothing -> error "editor has no modes assigned"

updateEditorTitle :: SourceView -> IO ()
updateEditorTitle editor = do
    Just scrolledW <- widgetGetParent editor
    Just notebookW <- widgetGetParent scrolledW
    let notebook = castToNotebook notebookW
    notebookSetTabLabelText notebook scrolledW =<< tabTitleForEditor editor

fileNameQuark :: IO Quark
fileNameQuark = quarkFromString "fileName"

editorIdQuark :: IO Quark
editorIdQuark = quarkFromString "editorId"

editorModesQuark :: IO Quark
editorModesQuark = quarkFromString "editorModes"
