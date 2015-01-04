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
import Data.Char (isPrint)
import Control.Concurrent.MVar (newMVar, readMVar, modifyMVar_, MVar)
import qualified Control.Monad.State.Lazy as S
import Hob.Context
import Hob.Context.UiContext

data SourceData = SourceData {
    isModified :: Bool,
    textLines  :: [String]
}

data FancyEditor = FancyEditor {
    sourceData         :: SourceData,
    cursorPos          :: (Int, Int, Int), -- X, Y, X_{navigation}
    getFilePath        :: IO (Maybe FilePath),
    lineHeight         :: Double,
    fontAscent         :: Double,
    fontDescent        :: Double
}

newSourceData :: Text -> SourceData
newSourceData text = SourceData
            { isModified = False
            , textLines = lines . unpack $ text
            }

toFancyEditor :: FontMetrics -> DrawingArea -> Text -> FancyEditor
toFancyEditor fontSizeInfo widget text = FancyEditor
            { sourceData = newSourceData text
            , cursorPos = (0, 0, 0)
            , getFilePath = getEditorWidgetFilePath widget
            , lineHeight = (ascent fontSizeInfo) + (descent fontSizeInfo) + 5
            , fontAscent = ascent fontSizeInfo
            , fontDescent = descent fontSizeInfo
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

            pangoContext <- cairoCreateContext Nothing
            fontDescription <- fontDescriptionNew
            fontDescriptionSetFamily fontDescription "Ubuntu"
            fontDescriptionSetSize fontDescription 18
            contextSetFontDescription pangoContext fontDescription
            fontSizeInfo <- contextGetMetrics pangoContext fontDescription emptyLanguage

            fancyEditorDataHolder <- newMVar $ toFancyEditor fontSizeInfo editorWidget text

            _ <- editorWidget `on` keyPressEvent $ keyEventHandler fancyEditorDataHolder editorWidget pangoContext scrolledWindow
            _ <- editorWidget `on` draw $ drawEditor pangoContext fancyEditorDataHolder editorWidget
            _ <- editorWidget `on` buttonPressEvent $ liftIO $ widgetGrabFocus editorWidget >> return True

            return $ toEditor editorWidget

            
nrOfCharsOnCursorLine :: Int -> S.StateT FancyEditor IO Int
nrOfCharsOnCursorLine yPos = S.gets sourceData >>= return . maybe 0 length . listToMaybe . take 1 . drop yPos . textLines

clampCursorX :: Int -> Int -> S.StateT FancyEditor IO Int
clampCursorX yPos pos
 | pos < 0 = return 0
 | otherwise = nrOfCharsOnCursorLine yPos >>= (return . min pos)

clampCursorY :: Int -> S.StateT FancyEditor IO Int
clampCursorY pos
 | pos < 0 = return 0
 | otherwise = nrOfLines >>= (return . min pos)
 where 
    nrOfLines = S.gets sourceData >>= return . length . textLines

updateCursorX :: Int -> S.StateT FancyEditor IO ()
updateCursorX delta = do
    (cx, cy, _) <- S.gets cursorPos
    cx' <- clampCursorX cy $ cx + delta
    S.modify $ \ed -> ed{cursorPos = (cx', cy, cx')}

updateCursorY :: Int -> S.StateT FancyEditor IO ()
updateCursorY delta = do
    (_, cy, cxn) <- S.gets cursorPos
    cy' <- clampCursorY $ cy + delta
    cx' <- clampCursorX cy' cxn
    S.modify $ \ed -> ed{cursorPos = (cx', cy', cxn)}

modifyEditor :: MVar FancyEditor -> S.StateT FancyEditor IO () -> IO ()
modifyEditor fancyEditorDataHolder comp = modifyMVar_ fancyEditorDataHolder $ S.execStateT comp

cursorPosToXY :: [[[Double]]] -> Int -> Int -> S.StateT FancyEditor IO (Double, Double)
cursorPosToXY lineWidths cx cy = do
    h <- S.gets lineHeight
    a <- S.gets fontAscent
    return (cursorTop h a, cursorLeft)
    where
        cursorTop h a = (fromIntegral cy) * h - a
        maybeCursorLine = listToMaybe $ take 1 . drop cy $ lineWidths
        cursorLeft = maybe 0 (sum . take cx . concat) maybeCursorLine

scrollEditorToCursor :: ScrolledWindowClass s => PangoContext -> s -> S.StateT FancyEditor IO ()
scrollEditorToCursor pangoContext scrolledWindow = do
    lHeight <- S.gets lineHeight
    (cursorTop, cursorLeft) <- getCursorPositionXY pangoContext
    liftIO $ do
        hAdj <- scrolledWindowGetHAdjustment scrolledWindow
        adjustmentClampPage hAdj cursorLeft (cursorLeft+30)
        vAdj <- scrolledWindowGetVAdjustment scrolledWindow
        adjustmentClampPage vAdj cursorTop (cursorTop+2*lHeight+30)

getCursorPositionXY :: PangoContext -> S.StateT FancyEditor IO (Double, Double)
getCursorPositionXY pangoContext = do
    source <- S.gets sourceData
    (cursorCharNr, cursorLineNr, _) <- S.gets cursorPos
    let linesToDraw = textLines source
    (_, lineWidths) <- liftIO $ getLineShapesWithWidths pangoContext linesToDraw
    cursorPosToXY lineWidths cursorCharNr cursorLineNr
        
insertEditorChar :: Char -> S.StateT FancyEditor IO ()
insertEditorChar c = do
    source <- S.gets sourceData
    (cx, cy, _) <- S.gets cursorPos
    let tLines = textLines source
        (preLines, postLines) = splitAt cy tLines
        changedLine = fmap (\l -> let (preChars, postChars) = splitAt cx l
                                  in preChars ++ [c] ++ postChars
                            ) . take 1 $ postLines
        tLines' = preLines ++ changedLine ++ (drop 1 postLines)
        source' = source{textLines = tLines', isModified = True}
    S.modify $ \ed -> ed{sourceData = source'}

keyEventHandler :: (ScrolledWindowClass s, WidgetClass w) => MVar FancyEditor -> w -> PangoContext -> s -> EventM EKey Bool
keyEventHandler fancyEditorDataHolder editorWidget pangoContext scrolledWindow = do
    modifiers <- eventModifier
    keyValue <- eventKeyVal
    let printableChar = (\c -> if isPrint c then Just c else Nothing) =<< keyToChar keyValue
    -- TODO: widgetQueueDraw should only redraw previous and next cursor regions 
    -- TODO: handle delete, backspace, enter
    -- TODO: handle home, end, page down, page up
    -- TODO: handle text selection, cut, copy, paste
    let invokeEditorCmd :: S.StateT FancyEditor IO () -> EventM EKey Bool
        invokeEditorCmd cmd  = liftIO $ do
            modifyEditor fancyEditorDataHolder $ do
                cmd
                scrollEditorToCursor pangoContext scrolledWindow
            widgetQueueDraw editorWidget
            return True

    case (modifiers, unpack $ keyName keyValue) of
        ([], "Right") -> invokeEditorCmd $ updateCursorX 1
        ([], "Left") -> invokeEditorCmd $ updateCursorX (-1)
        ([], "Up") -> invokeEditorCmd $ updateCursorY (-1)
        ([], "Down") -> invokeEditorCmd $ updateCursorY 1
        _ -> maybe (return False) (\c -> invokeEditorCmd $ insertEditorChar c >> updateCursorX 1) printableChar


getLineShapesWithWidths :: PangoContext -> [String] -> IO ([[GlyphItem]], [[[Double]]])
getLineShapesWithWidths pangoContext linesToDraw = do
    pangoLineShapes <- sequence $ map (\line -> do
            pangoItems <- pangoItemize pangoContext line []
            sequence $ map pangoShape pangoItems                        
        ) linesToDraw

    lineWidths <- sequence . map (sequence . map (`glyphItemGetLogicalWidths` Nothing)) $ pangoLineShapes
    return (pangoLineShapes, lineWidths)

getBoundingRect :: [(Double, [[Double]], [GlyphItem])] -> S.StateT FancyEditor IO (Double, Double, Double, Double)
getBoundingRect drawableLines = do
    a <- S.gets fontAscent
    h <- S.gets lineHeight
    return (textLeft, textTop a, textRight, textBottom a h)
    where
        textLeft = 7
        textTop a = 5 + a
        textRight = 7 + textLeft + (if null drawableLines then 0 else maximum . map (sum . map sum) . drawableLineWidths $ drawableLines)
        textBottom a h = textTop a + (if null drawableLines then 0 else (h + (lineTop . last $ drawableLines)))
        lineTop (t, _, _) = t

drawableLineWidths :: [(t, b, t1)] -> [b]
drawableLineWidths = map (\(_, w, _) -> w)

getDrawableLineData :: PangoContext -> S.StateT FancyEditor IO [(Double, [[Double]], [GlyphItem])]
getDrawableLineData pangoContext = do
    source <- S.gets sourceData
    h <- S.gets lineHeight
    let linesToDraw = textLines source
    (pangoLineShapes, lineWidths) <- liftIO $ getLineShapesWithWidths pangoContext linesToDraw
    return $ zip3 [i*h | i <- [0..]] lineWidths pangoLineShapes


drawEditor :: WidgetClass w => PangoContext -> MVar FancyEditor -> w -> Render ()
drawEditor pangoContext fancyEditorDataHolder editorWidget = do
        drawData <- getDrawableData
        drawBackground
        drawContents drawData
    where
        drawBackground = do
            setSourceRGB 0.86 0.85 0.8
            paint
    
        drawContents (drawableLines, (textLeft, textTop, textRight, textBottom), cursorPosData) = do
            liftIO $ widgetSetSizeRequest editorWidget (ceiling textRight) (ceiling textBottom)
            save
            translate textLeft textTop
            drawText drawableLines
            drawCursor cursorPosData
            restore

        drawText drawableLines = do
            setSourceRGBA 0 0.1 0 1
            sequence_ $ map (\(yPos, _, pangoShapes) -> do
                    moveTo 0 yPos
                    sequence_ $ map showGlyphString pangoShapes
                ) drawableLines

        drawCursor (cursorTop, cursorLeft, cursorBottom) = do
            setLineWidth 1
            setSourceRGBA 0 0 0.3 0.8
            moveTo cursorLeft cursorTop
            lineTo cursorLeft cursorBottom
            stroke
        
        getDrawableData = liftIO $ S.evalStateT (do
                    drawableLines <- getDrawableLineData pangoContext
                    boundingRect <- getBoundingRect drawableLines
                    (cursorCharNr, cursorLineNr, _) <- S.gets cursorPos
                    (cursorTop, cursorLeft) <- cursorPosToXY (drawableLineWidths drawableLines) cursorCharNr cursorLineNr
                    a <- S.gets fontAscent
                    d <- S.gets fontDescent
                    return (drawableLines, boundingRect, (cursorTop, cursorLeft, cursorTop+a+d))
                ) =<< readMVar fancyEditorDataHolder


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
