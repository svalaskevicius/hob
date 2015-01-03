module Hob.Ui.Editor.Fancy (
    newEditorForText
    ) where

import Control.Monad.Reader
import Data.Text                  (Text, unpack)
import Filesystem.Path.CurrentOS  (decodeString, encodeString, filename)
import Graphics.UI.Gtk
import System.Glib.GObject        (Quark)
import Graphics.Rendering.Cairo

import Hob.Context
import Hob.Context.UiContext

data SourceData = SourceData {
    isModified :: Bool,
    textLines  :: [String]
}

data FancyEditor = FancyEditor {
    sourceData         :: SourceData,
    getFilePath        :: IO (Maybe FilePath)
}

newSourceData :: Text -> SourceData
newSourceData text = SourceData
            { isModified = False
            , textLines = lines . unpack $ text
            }

toFancyEditor :: DrawingArea -> Text -> FancyEditor
toFancyEditor widget text = FancyEditor
            { sourceData = newSourceData text
            , getFilePath = getEditorWidgetFilePath widget
            }

toEditor :: DrawingArea -> Text -> Editor
toEditor widget text = Editor
            { editorId = const . liftIO $ getEditorId widget

            , enterEditorMode = \editor mode -> do
                modes <- liftIO $ getEditorModes widget
                liftIO $ setEditorModes widget $ modes++[mode]
                return editor

            , exitLastEditorMode = \editor -> do
                modes <- liftIO $ getEditorModes widget
                if null modes then return editor
                else do
                    cleanup $ last modes
                    liftIO $ setEditorModes widget $ init modes
                    return editor

            , modeStack  = const . liftIO $ getEditorModes widget

            , isCurrentlyActive = const $ do
                ctx <- ask
                active <- liftIO $ getActiveEditor ctx
                return $ active == Just widget
            }
    where
        fancyEditor = toFancyEditor widget text


newEditorForText :: Notebook -> Maybe FilePath -> Text -> App ()
newEditorForText targetNotebook filePath text = do
    ctx <- ask
    liftIO $ do
        editor <- createNewEditor ctx
        updateEditors (editors ctx) $ \oldEditors -> return $ oldEditors ++ [editor]
    where
        title = tabTitleForFile filePath
        createNewEditor ctx = do
            editorWidget <- drawingAreaNew
            widgetSetSizeRequest editorWidget 500 500
            newId <- idGenerator ctx
            setEditorId editorWidget newId
            setEditorWidgetFilePath editorWidget filePath
            setEditorModes editorWidget []

            scrolledWindow <- scrolledWindowNew Nothing Nothing
            scrolledWindow `containerAdd` editorWidget
            widgetShowAll scrolledWindow
            tabNr <- notebookAppendPage targetNotebook scrolledWindow title
            notebookSetCurrentPage targetNotebook tabNr
            notebookSetShowTabs targetNotebook True

            let fancyEditorData = toFancyEditor editorWidget text

            _ <- editorWidget `on` draw $ do
                setSourceRGB 0.96 0.95 0.9
                paint
                setSourceRGBA 0 0 0 1
                selectFontFace "Verdana" FontSlantNormal FontWeightNormal
                setFontSize 18
                fontSizeInfo <- fontExtents
                let fontHeight = fontExtentsHeight fontSizeInfo
                    lineHeight = fontHeight + 5
                    posWithText = zip [i*lineHeight | i <- [0..]] (textLines $ sourceData fancyEditorData)
                    
                save
                translate 7 (5 + (fontExtentsAscent fontSizeInfo))
                sequence_ $ map (\(yPos, line) -> do
                        moveTo 0 yPos
                        showText line
                    ) posWithText
                restore
            return $ toEditor editorWidget text

{-
getActiveEditorText :: Context -> IO (Maybe Text)
getActiveEditorText ctx = do
    editor <- getActiveEditor ctx
    maybe (return Nothing) ((return . Just) <=< getEditorText) editor

getEditorText :: TextViewClass a => a -> IO Text
getEditorText textEdit = do
    textBuf <- textViewGetBuffer textEdit
    get textBuf textBufferText

invokeOnActiveEditor :: (SourceView -> IO()) -> App ()
invokeOnActiveEditor actions = do
    ctx <- ask
    editor <- liftIO $ getActiveEditor ctx
    liftIO $ maybeDo actions editor
-}

getActiveEditor :: Context -> IO (Maybe DrawingArea)
getActiveEditor = maybe (return Nothing) getEditorFromNotebookTab <=< getActiveEditorTab

getEditorFromNotebookTab :: Widget -> IO (Maybe DrawingArea)
getEditorFromNotebookTab currentlyActiveEditor =
    if currentlyActiveEditor `isA` gTypeScrolledWindow then do
        let textEditScroller = castToScrolledWindow currentlyActiveEditor
        textEdit <- binGetChild textEditScroller
        let areaa = ((\area -> if isA area gTypeDrawingArea then Just area else Nothing) =<< textEdit)
        return $ fmap castToDrawingArea areaa
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
{-
tabTitleForEditor :: Editor -> IO String
tabTitleForEditor editor = do
    filePath <- getFilePath editor
    let modified = isModified $ textData editor
    return $ if modified then tabTitleForFile filePath ++ "*" else tabTitleForFile filePath
-}
setEditorWidgetFilePath :: WidgetClass a => a -> Maybe FilePath -> IO ()
setEditorWidgetFilePath editor filePath = do
    quark <- fileNameQuark
    objectSetAttribute quark editor filePath

getEditorWidgetFilePath :: WidgetClass a => a -> IO (Maybe FilePath)
getEditorWidgetFilePath editor = do
    quark <- fileNameQuark
    objectGetAttributeUnsafe quark editor

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
{-
updateEditorTitle :: WidgetClass a => a -> Editor -> IO ()
updateEditorTitle editorWidget editor = do
    Just scrolledW <- widgetGetParent editorWidget
    Just notebookW <- widgetGetParent scrolledW
    let notebook = castToNotebook notebookW
    notebookSetTabLabelText notebook scrolledW =<< tabTitleForEditor editor
-}
fileNameQuark :: IO Quark
fileNameQuark = quarkFromString "fileName"

editorIdQuark :: IO Quark
editorIdQuark = quarkFromString "editorId"

editorModesQuark :: IO Quark
editorModesQuark = quarkFromString "editorModes"
