module Hob.Ui.Editor.Fancy (
    newEditorForText, getActiveEditorWidget, initModule
    ) where

import           Control.Concurrent.MVar             (MVar, putMVar, newEmptyMVar,
                                                      readMVar)
import           Control.Monad.Reader
import qualified Control.Monad.State.Lazy            as S
import           Data.Maybe                          (mapMaybe, fromMaybe)
import           Data.Text                           (Text)
import    qualified       Data.Text                as T
import Data.Text.Internal.Search (indices)

import           Filesystem.Path.CurrentOS           (decodeString,
                                                      encodeString, filename)
import           Graphics.UI.Gtk                     hiding (Point)
import Graphics.UI.Gtk.General.Enums
import           System.Glib.GObject                 (Quark)
import qualified Graphics.UI.Gtk.General.StyleContext as GtkSc

import           Hob.Context
import           Hob.Control
import           Hob.Context.UiContext

import Hob.Ui.Editor.Fancy.Types
import Hob.Ui.Editor.Fancy.Commands
import Hob.Ui.Editor.Fancy.Shapes
import Hob.Ui.Editor.Fancy.Renderer

initModule :: App()
initModule = do
    registerParametrisedEventHandler "editor.filePath.change" updateEditorTitle
    registerParametrisedEventHandler "editor.modified.change" updateEditorTitle
    where
        updateEditorTitle :: (MVar FancyEditor, Editor) -> App()
        updateEditorTitle (fancyEditorHolder, editorApi) = do
            filePath <- getEditorFilePath editorApi editorApi
            facyEditor <- liftIO $ readMVar fancyEditorHolder
            let modifiedState = isModified . sourceData $ facyEditor
                widget = drawingAreaWidget facyEditor
            liftIO $ setEditorTitle widget (tabTitleForFile filePath modifiedState)



-- TODO: maybe add pango context, widget, scrolling widget
newFancyEditor :: DrawingArea -> Int -> Label -> PangoContext -> Text -> App (MVar FancyEditor)
newFancyEditor widget newEditorId errLabel pangoContext text = do
    ctx <- ask
    liftIO $ do
        holder <- newEmptyMVar
        putMVar holder =<< createFancyEditor ctx holder
        return holder
    where
        createFancyEditor ctx holder = do
            dOptions <- newDrawingOptions pangoContext errorReporter
            sd <- newSourceDataFromText text
            dd <- newDrawingData Nothing pangoContext sd initialCursor initialCursor dOptions
            return FancyEditor {
                sourceData = sd,
                cursorHead = initialCursor,
                selectionHead = Nothing,
                drawingOptions = dOptions,
                drawingData = dd,
                editorApiId = newEditorId,
                emitInternalEditorEvent = \name ->
                    deferredRunner ctx $ do
                        me <- editorById newEditorId
                        case me of
                            Just e -> emitParametrisedEvent name (holder, e)
                            Nothing -> return(),
                emitEditorEvent = \name ->
                    deferredRunner ctx $ do
                        me <- editorById newEditorId
                        case me of
                            Just e -> emitParametrisedEvent name e
                            Nothing -> return(),
                drawingAreaWidget = widget,
                redraw = do
                    updateDrawingData pangoContext
                    liftIO $ widgetQueueDraw widget
            }
        initialCursor = CursorHead 0 0 0
        errorReporter (Just err) = do
            labelSetText errLabel err
            widgetShowAll errLabel
        errorReporter Nothing = widgetHide errLabel

toEditor :: DrawingArea -> Maybe FilePath -> MVar FancyEditor -> Editor
toEditor widget filePath fancyEditorDataHolder = Editor {
    editorId = const . liftIO $ getEditorId widget
    ,
    enterEditorMode = \editor mode -> do
        modes <- liftIO $ getEditorModes widget
        liftIO $ setEditorModes widget $ modes++[mode]
        return editor
    ,
    exitLastEditorMode = \editor -> do
        modes <- liftIO $ getEditorModes widget
        if null modes then return editor
        else do
            cleanup $ last modes
            liftIO $ setEditorModes widget $ init modes
            return editor
    ,
    modeStack  = const . liftIO $ getEditorModes widget
    ,
    isCurrentlyActive = const $ do
        ctx <- ask
        active <- liftIO $ getActiveEditor ctx
        return $ active == Just widget
    ,
    getEditorFilePath = \_ -> return filePath
    ,
    setEditorFilePath = \editor filePath' -> let editor' = editor{ getEditorFilePath =  \_ -> return filePath' }
                                             in do
                                                 defer $ emitParametrisedEvent "editor.filePath.change" (fancyEditorDataHolder, editor')
                                                 return editor'
    ,
    getEditorContents = \_ -> liftIO $ do
        fancyEditorData <- liftIO $ readMVar fancyEditorDataHolder
        let text = T.unlines . textLines . sourceData $ fancyEditorData
        return text
    ,
    activateEditor = \_ notebook -> liftIO $ do
        currentEditors <- mapM getEditorFromNotebookTab <=< containerGetChildren $ notebook
        let editorsForFile = take 1 . filter (\(_, ed) -> widget == ed ) . numberedJusts $ currentEditors
        case editorsForFile of
            [(nr, _)] ->  notebookSetCurrentPage notebook nr
            _ -> return()
    ,
    setModifiedState = \editor newState -> do
        liftIO $ modifyEditor fancyEditorDataHolder $ setEditorModifiedState newState
        return editor
    ,
    highlightSearchPreview = \editor text -> liftIO $ do
        liftIO $ modifyEditor fancyEditorDataHolder $ fancyHighlightSearchPreview (T.pack text)
        return editor
    ,
    resetSearchPreview = liftIO . return
--        error "not implemented"
    ,
    findFirstFromCursor = \editor text -> liftIO $ do
        error "not implemented"
        return editor
    ,
    findNext = \editor -> liftIO $ do
        error "not implemented"
        return editor
    ,
    findPrevious = \editor -> liftIO $ do
        error "not implemented"
        return editor
    ,
    resetSearch = \editor -> liftIO $ do
        error "not implemented"
        return editor
    ,
    startReplace = \editor search replace -> liftIO $ do
        error "not implemented"
        return editor
    ,
    replaceNext = \editor -> liftIO $ do
        error "not implemented"
        return editor
    ,
    resetReplace = \editor -> liftIO $ do
        error "not implemented"
        return editor

}

fancyHighlightSearchPreview :: Text -> S.StateT FancyEditor IO ()
fancyHighlightSearchPreview text
 | T.null text = error "empty text to preview"
 | otherwise = do
    source <- S.gets sourceData
    let tLines = textLines source
        len = T.length text
        foundIndices = map (map (flip (,) len) . indices text) tLines
        source' = source{textHighlights = foundIndices}
    S.modify $ \ed -> ed{sourceData = source'}
    join $ S.gets redraw


numberedJusts :: [Maybe a] -> [(Int, a)]
numberedJusts a = mapMaybe liftTupledMaybe $ zip [0..] a

liftTupledMaybe :: (a, Maybe b) -> Maybe (a, b)
liftTupledMaybe (xx, Just xy) = Just (xx, xy)
liftTupledMaybe (_, Nothing) = Nothing

setEditorTitle :: DrawingArea -> String -> IO ()
setEditorTitle editorWidget title = do
    mNotebook <- findParentNotebook (castToWidget editorWidget)
    maybeDo (\(notebook, tabWidget) -> notebookSetTabLabelText notebook tabWidget title) mNotebook

    where
        findParentNotebook :: Widget -> IO (Maybe (Notebook, Widget))
        findParentNotebook widget = do
            mParent <- widgetGetParent widget
            case mParent of
                Just parent -> if parent `isA` gTypeNotebook then
                                   return $ Just (castToNotebook parent, widget)
                               else findParentNotebook parent
                Nothing -> return Nothing

newEditorForText :: Notebook -> Maybe FilePath -> Text -> App ()
newEditorForText targetNotebook filePath text = do
    ctx <- ask
    liftIO $ do
        editor <- createNewEditor ctx
        updateEditors (editors ctx) $ \oldEditors -> return $ oldEditors ++ [editor]
    where
        title = tabTitleForFile filePath False

        newEditorWidget newId = do
            editorWidget <- drawingAreaNew
            widgetSetCanFocus editorWidget True
            setEditorId editorWidget newId
            setEditorWidgetFilePath editorWidget filePath
            setEditorModes editorWidget []
            return editorWidget

        newEditorScrolls editorWidget = do
            scrolledWindow <- scrolledWindowNew Nothing Nothing
            scrolledWindow `containerAdd` editorWidget
            return scrolledWindow

        newPangoContext = do
            pangoContext <- cairoCreateContext Nothing
            fontDescription <- fontDescriptionNew
            fontDescriptionSetFamily fontDescription "Monaco"
            fontDescriptionSetSize fontDescription 12
            contextSetFontDescription pangoContext fontDescription
            return pangoContext

        createNewEditor ctx = do
            newEditorId <- idGenerator ctx
            editorWidget <- newEditorWidget newEditorId
            scrolledWindow <- newEditorScrolls editorWidget

            overl <- overlayNew
            errorLabel <- labelNew (Just "")
            labeltyleContext <- widgetGetStyleContext errorLabel
            GtkSc.styleContextAddClass labeltyleContext "error"
            widgetSetVAlign errorLabel AlignEnd
            widgetSetHAlign errorLabel AlignStart
            containerAdd overl scrolledWindow
            overlayAdd overl errorLabel

            widgetShowAll overl
            widgetHide errorLabel

            tabNr <- notebookAppendPage targetNotebook overl title
            notebookSetCurrentPage targetNotebook tabNr
            notebookSetShowTabs targetNotebook True

            pangoContext <- newPangoContext

            fancyEditorDataHolder <- runApp ctx $ newFancyEditor editorWidget newEditorId errorLabel pangoContext text

            _ <- editorWidget `on` keyPressEvent $ keyEventHandler fancyEditorDataHolder editorWidget pangoContext scrolledWindow
            _ <- editorWidget `on` draw $ drawEditor fancyEditorDataHolder editorWidget
            _ <- editorWidget `on` buttonPressEvent $ liftIO $ widgetGrabFocus editorWidget >> return True

            let editor = toEditor editorWidget filePath fancyEditorDataHolder

            return editor

-- | TODO: invalidate and update partially
updateDrawingData :: PangoContext -> S.StateT FancyEditor IO ()
updateDrawingData pangoContext = do
    source <- S.gets sourceData
    cursor <- S.gets cursorHead
    selection <- S.gets selectionHead
    opts <- S.gets drawingOptions
    dData <- S.gets drawingData
    dData' <- liftIO $ newDrawingData (Just dData) pangoContext source cursor (fromMaybe cursor selection) opts
    S.modify $ \ed -> ed{drawingData = dData'}


getActiveEditor :: Context -> IO (Maybe DrawingArea)
getActiveEditor = maybe (return Nothing) getEditorFromNotebookTab <=< getActiveEditorTab

getActiveEditorWidget :: Context -> IO (Maybe Widget)
getActiveEditorWidget = maybe (return Nothing) getEditorWidgetFromNotebookTab <=< getActiveEditorTab

getEditorWidgetFromNotebookTab :: Widget -> IO (Maybe Widget)
getEditorWidgetFromNotebookTab = recurseInBin
    where
        binChild widget = if widget `isA` gTypeBin then
                                  binGetChild $ castToBin widget
                              else return Nothing

        recurseInBin widget = do
                                  mChild <- binChild widget
                                  case mChild of
                                      Just child -> recurseInBin child
                                      Nothing -> return $ Just widget

getEditorFromNotebookTab :: Widget -> IO (Maybe DrawingArea)
getEditorFromNotebookTab mainWidget = asDrawingArea <~=< getEditorWidgetFromNotebookTab mainWidget
    where
        asDrawingArea widget = if widget `isA` gTypeDrawingArea then
                                   return $ Just $ castToDrawingArea widget
                               else
                                   return Nothing

        target <~=< source = do
            res <- source
            maybe (return Nothing) target res
        infixr 9 <~=<

getActiveEditorTab :: Context -> IO (Maybe Widget)
getActiveEditorTab ctx = do
    pageNum <- notebookGetCurrentPage tabbed
    if pageNum < 0 then
        return Nothing
    else do
        tabs <- containerGetChildren tabbed
        return $ Just $ tabs!!pageNum
    where tabbed = mainNotebook.uiContext $ ctx

tabTitleForFile :: Maybe FilePath -> Bool -> String
tabTitleForFile mFilePath modifiedState = if modifiedState then baseTitle mFilePath ++ "*" else baseTitle mFilePath
    where
        baseTitle (Just filePath) = filename' filePath
        baseTitle Nothing = "(new file)"
        filename' = encodeString . filename . decodeString

setEditorWidgetFilePath :: WidgetClass a => a -> Maybe FilePath -> IO ()
setEditorWidgetFilePath editor filePath = do
    quark <- fileNameQuark
    objectSetAttribute quark editor filePath

setEditorId :: WidgetClass a => a -> Int -> IO ()
setEditorId editor identifier = do
    quark <- editorIdQuark
    objectSetAttribute quark editor $ Just identifier

getEditorId :: WidgetClass a => a -> IO Int
getEditorId editor = do
    quark <- editorIdQuark
    mRet <- objectGetAttributeUnsafe quark editor
    case mRet of
        Just ret -> return ret
        Nothing -> error "editor has no id assigned"

setEditorModes :: WidgetClass a => a -> [Mode] -> IO ()
setEditorModes editor modes = do
    quark <- editorModesQuark
    objectSetAttribute quark editor $ Just modes

getEditorModes :: WidgetClass a => a -> IO [Mode]
getEditorModes editor = do
    quark <- editorModesQuark
    mRet <- objectGetAttributeUnsafe quark editor
    case mRet of
        Just ret -> return ret
        Nothing -> error "editor has no modes assigned"

fileNameQuark :: IO Quark
fileNameQuark = quarkFromString "fileName"

editorIdQuark :: IO Quark
editorIdQuark = quarkFromString "editorId"

editorModesQuark :: IO Quark
editorModesQuark = quarkFromString "editorModes"
