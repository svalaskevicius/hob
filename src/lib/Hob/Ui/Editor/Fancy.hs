module Hob.Ui.Editor.Fancy (
    newEditorForText
    ) where

import Control.Monad.Reader
import Data.Text                  (Text, unpack)
import Filesystem.Path.CurrentOS  (decodeString, encodeString, filename)
import Graphics.UI.Gtk
import System.Glib.GObject        (Quark)
import Graphics.Rendering.Cairo
import Data.Maybe (listToMaybe)
import Control.Concurrent.MVar (newMVar, readMVar, modifyMVar_)

import Hob.Context
import Hob.Context.UiContext

data SourceData = SourceData {
    isModified :: Bool,
    textLines  :: [String]
}

data FancyEditor = FancyEditor {
    sourceData         :: SourceData,
    cursorPos          :: (Int, Int, Int), -- X, Y, X_{navigation}
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
            , cursorPos = (0, 0, 0)
            , getFilePath = getEditorWidgetFilePath widget
            }

toEditor :: DrawingArea -> Editor
toEditor widget = Editor
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
            widgetSetCanFocus editorWidget True
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

            fancyEditorDataHolder <- newMVar $ toFancyEditor editorWidget text
            
            let nrOfCharsOnCursorLine fancyEditorData yPos = maybe 0 length . listToMaybe . take 1 . drop yPos $ editorTextLines
                    where
                        editorTextLines = textLines $ sourceData fancyEditorData
                clampCursorX fancyEditorData yPos pos
                 | pos < 0 = 0
                 | pos > nrOfChars = nrOfChars
                 | otherwise = pos
                 where 
                    nrOfChars = nrOfCharsOnCursorLine fancyEditorData yPos
                clampCursorY fancyEditorData pos
                 | pos < 0 = 0
                 | pos > nrOfLines = nrOfLines
                 | otherwise = pos
                 where 
                    nrOfLines = length . textLines . sourceData $ fancyEditorData
            let updateCursorX delta = modifyMVar_ fancyEditorDataHolder $ \fancyEditorData -> do
                                            let (cx, cy, _) = cursorPos fancyEditorData
                                                cx' = clampCursorX fancyEditorData cy $ cx + delta
                                            return fancyEditorData{cursorPos = (cx', cy, cx')}
            let updateCursorY delta = modifyMVar_ fancyEditorDataHolder $ \fancyEditorData -> do
                                            let (_, cy, cxn) = cursorPos fancyEditorData
                                                cy' = clampCursorY fancyEditorData $ cy + delta
                                                cx' = clampCursorX fancyEditorData cy' cxn
                                            return fancyEditorData{cursorPos = (cx', cy', cxn)}


            pangoContext <- cairoCreateContext Nothing
            fontDescription <- fontDescriptionNew
            fontDescriptionSetFamily fontDescription "Ubuntu"
            fontDescriptionSetSize fontDescription 18
            contextSetFontDescription pangoContext fontDescription

            _ <- editorWidget `on` keyPressEvent $ do
                modifiers <- eventModifier
                keyValue <- eventKeyVal
                -- TODO: widgetQueueDraw should only redraw previous and next cursor regions 
                -- TODO: scroll the whole view to fit cursor on screen
                let moveEditorCursor cmd  = liftIO $ cmd >> widgetQueueDraw editorWidget >> return True
                case (modifiers, unpack $ keyName keyValue) of
                    ([], "Right") -> moveEditorCursor $ updateCursorX 1
                    ([], "Left") -> moveEditorCursor $ updateCursorX (-1)
                    ([], "Up") -> moveEditorCursor $ updateCursorY (-1)
                    ([], "Down") -> moveEditorCursor $ updateCursorY 1
                    _ -> return False

            _ <- editorWidget `on` draw $ do
                setSourceRGB 0.86 0.85 0.8
                paint
                
                setSourceRGBA 0 0.1 0 1
                fontSizeInfo <- liftIO $ contextGetMetrics pangoContext fontDescription emptyLanguage
                fancyEditorData <- liftIO $ readMVar fancyEditorDataHolder
                let linesToDraw = textLines $ sourceData fancyEditorData
                pangoLineShapes <- liftIO $ sequence $ map (\line -> do
                        pangoItems <- pangoItemize pangoContext line []
                        sequence $ map pangoShape pangoItems                        
                    ) linesToDraw

                lineWidths <- liftIO $ sequence . map (sequence . map (`glyphItemGetLogicalWidths` Nothing)) $ pangoLineShapes
                let fontHeight = ascent fontSizeInfo + descent fontSizeInfo
                    lineHeight = fontHeight + 5
                    posWithShapes = zip [i*lineHeight | i <- [0..]] pangoLineShapes
                    textLeft :: Double
                    textLeft = 7
                    textTop = 5 + (ascent fontSizeInfo)
                    textBottom = textTop + (if null posWithShapes then 0 else lineHeight + (fst . last $ posWithShapes))
                    textRight = 7 + textLeft + (if null lineWidths then 0 else maximum . map (sum . map sum) $ lineWidths)

                liftIO $ widgetSetSizeRequest editorWidget (ceiling textRight) (ceiling textBottom)

                save
                translate textLeft textTop

                sequence_ $ map (\(yPos, pangoShapes) -> do
                        moveTo 0 yPos
                        sequence_ $ map showGlyphString pangoShapes
                    ) posWithShapes

                let (cursorCharNr, cursorLineNr, _) = cursorPos $ fancyEditorData
                    cursorTop = (fromIntegral cursorLineNr) * lineHeight - (ascent fontSizeInfo)
                    maybeCursorLine = listToMaybe $ take 1 . drop cursorLineNr $ lineWidths
                    cursorLeft = maybe 0 (sum . take cursorCharNr . concat) maybeCursorLine

                setLineWidth 1
                setSourceRGBA 0 0 0.3 0.8
                moveTo cursorLeft cursorTop
                lineTo cursorLeft (cursorTop + lineHeight)
                stroke
                
                restore
                
            _ <- editorWidget `on` buttonPressEvent $ do
                liftIO $ widgetGrabFocus editorWidget
                return True

            return $ toEditor editorWidget

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
