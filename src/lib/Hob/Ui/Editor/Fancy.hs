module Hob.Ui.Editor.Fancy (
    newEditorForText
    ) where

import Control.Monad.Reader
import Data.Text                  (Text, unpack)
import Filesystem.Path.CurrentOS  (decodeString, encodeString, filename)
import Graphics.UI.Gtk hiding (Point)
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

-- | Point x y
data Point = Point Double Double

-- | DrawableLine (y position) (x position of each char) (glyphs to draw)
data DrawableLine = DrawableLine Double [Double] [GlyphItem]

data EditorDrawingData = EditorDrawingData {
    drawableLines      :: [DrawableLine],
    boundingRect       :: (Point, Point),
    cursorPosition     :: Point
}

data EditorDrawingOptions = EditorDrawingOptions {
    lineHeight         :: Double,
    fontAscent         :: Double,
    fontDescent        :: Double
}

data FancyEditor = FancyEditor {
    sourceData         :: SourceData,
    cursorPos          :: (Int, Int, Int), -- X, Y, X_{navigation}
    drawingOptions     :: EditorDrawingOptions,
    drawingData        :: EditorDrawingData
}

-- TODO: vector for lines?
newSourceData :: Text -> IO SourceData
newSourceData text = return $ SourceData {
    isModified = False,
    textLines = lines . unpack $ text
}

newDrawingData :: PangoContext -> SourceData -> (Int, Int, Int) -> EditorDrawingOptions -> IO EditorDrawingData
newDrawingData pangoContext source (cursorCharNr, cursorLineNr, _) opts = do
    lineData <- newDrawableLineData pangoContext source opts
    let cursorP = cursorPosToXY (drawableLineWidths lineData) cursorCharNr cursorLineNr opts
    return $ EditorDrawingData {
        drawableLines = lineData,
        boundingRect = getBoundingRect opts lineData,
        cursorPosition = cursorP
    }

newDrawingOptions :: PangoContext -> IO EditorDrawingOptions
newDrawingOptions pangoContext = do
    fontDescription <- contextGetFontDescription pangoContext
    fontSizeInfo <- contextGetMetrics pangoContext fontDescription emptyLanguage
    let a = ascent fontSizeInfo
        d = descent fontSizeInfo
    return $ EditorDrawingOptions {
        lineHeight = a + d + 5,
        fontAscent = a,
        fontDescent = d
    }

-- TODO: maybe add pango context, widget, scrolling widget
newFancyEditor :: PangoContext -> Text -> IO FancyEditor
newFancyEditor pangoContext text = do
    dOptions <- newDrawingOptions pangoContext
    sd <- newSourceData text
    dd <- newDrawingData pangoContext sd initialCursor dOptions
    return $ FancyEditor {
        sourceData = sd,
        cursorPos = initialCursor,
        drawingOptions = dOptions,
        drawingData = dd
    }
    where
        initialCursor = (0, 0, 0)

toEditor :: DrawingArea -> Editor
toEditor widget = Editor {
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

            fancyEditorDataHolder <- newMVar =<< newFancyEditor pangoContext text

            _ <- editorWidget `on` keyPressEvent $ keyEventHandler fancyEditorDataHolder editorWidget pangoContext scrolledWindow
            _ <- editorWidget `on` draw $ drawEditor fancyEditorDataHolder editorWidget
            _ <- editorWidget `on` buttonPressEvent $ liftIO $ widgetGrabFocus editorWidget >> return True

            return $ toEditor editorWidget

modifyEditor :: MVar FancyEditor -> S.StateT FancyEditor IO () -> IO ()
modifyEditor fancyEditorDataHolder comp = modifyMVar_ fancyEditorDataHolder $ S.execStateT comp

-- | TODO: invalidate and update partially
updateDrawingData :: PangoContext -> S.StateT FancyEditor IO ()
updateDrawingData pangoContext = do
    source <- S.gets sourceData
    cursor <- S.gets cursorPos
    opts <- S.gets drawingOptions
    dData <- liftIO $ newDrawingData pangoContext source cursor opts
    S.modify $ \ed -> ed{drawingData = dData}
            
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

cursorPosToXY :: [[Double]] -> Int -> Int -> EditorDrawingOptions -> Point
cursorPosToXY lineWidths cx cy opts = Point cursorLeft cursorTop
    where
        cursorTop = (fromIntegral cy) * lHeight - fAscent
        maybeCursorLine = listToMaybe $ take 1 . drop cy $ lineWidths
        cursorLeft = maybe 0 (sum . take cx) maybeCursorLine
        lHeight = lineHeight opts
        fAscent = fontAscent opts

scrollEditorToCursor :: ScrolledWindowClass s => s -> S.StateT FancyEditor IO ()
scrollEditorToCursor scrolledWindow = do
    opts <- S.gets drawingOptions
    let lHeight = lineHeight opts
    dData <- S.gets drawingData 
    let (Point cursorLeft cursorTop) = cursorPosition dData
    liftIO $ do
        hAdj <- scrolledWindowGetHAdjustment scrolledWindow
        adjustmentClampPage hAdj cursorLeft (cursorLeft+30)
        vAdj <- scrolledWindowGetVAdjustment scrolledWindow
        adjustmentClampPage vAdj cursorTop (cursorTop+2*lHeight+30)

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
                updateDrawingData pangoContext
                scrollEditorToCursor scrolledWindow
            widgetQueueDraw editorWidget
            return True

    case (modifiers, unpack $ keyName keyValue) of
        ([], "Right") -> invokeEditorCmd $ updateCursorX 1
        ([], "Left") -> invokeEditorCmd $ updateCursorX (-1)
        ([], "Up") -> invokeEditorCmd $ updateCursorY (-1)
        ([], "Down") -> invokeEditorCmd $ updateCursorY 1
        _ -> maybe (return False) (\c -> invokeEditorCmd $ insertEditorChar c >> updateCursorX 1) printableChar


getLineShapesWithWidths :: PangoContext -> [String] -> IO ([[GlyphItem]], [[Double]])
getLineShapesWithWidths pangoContext linesToDraw = do
    pangoLineShapes <- sequence $ map (\line -> do
            pangoItems <- pangoItemize pangoContext line []
            sequence $ map pangoShape pangoItems                        
        ) linesToDraw

    lineWordWidths <- sequence . map (sequence . map (`glyphItemGetLogicalWidths` Nothing)) $ pangoLineShapes
    return (pangoLineShapes, map concat lineWordWidths)

getBoundingRect :: EditorDrawingOptions -> [DrawableLine] -> (Point, Point)
getBoundingRect opts dLines = (Point textLeft textTop, Point textRight textBottom)
    where
        fAscent = fontAscent opts
        lHeight = lineHeight opts
        textLeft = 7
        textTop = 5 + fAscent
        textRight = 7 + textLeft + (if null dLines then 0 else maximum . map sum . drawableLineWidths $ dLines)
        textBottom = textTop + (if null dLines then 0 else (lHeight + (lineTop . last $ dLines)))
        lineTop (DrawableLine t _ _) = t

drawableLineWidths :: [DrawableLine] -> [[Double]]
drawableLineWidths = map (\(DrawableLine _ w _) -> w)

newDrawableLineData :: PangoContext -> SourceData -> EditorDrawingOptions -> IO [DrawableLine]
newDrawableLineData pangoContext source opts = do
    let linesToDraw = textLines source
    (pangoLineShapes, lineWidths) <- getLineShapesWithWidths pangoContext linesToDraw
    return $ map (\(a, b, c) -> DrawableLine a b c) $ zip3 [i*h | i <- [0..]] lineWidths pangoLineShapes
    where
        h = lineHeight opts

drawEditor :: WidgetClass w => MVar FancyEditor -> w -> Render ()
drawEditor fancyEditorDataHolder editorWidget = do
        drawData <- getDrawableData
        drawBackground
        drawContents drawData
    where
        drawBackground = do
            setSourceRGB 0.86 0.85 0.8
            paint
    
        drawContents (dData, opts) = do
            liftIO $ widgetSetSizeRequest editorWidget (ceiling textRight) (ceiling textBottom)
            save
            translate textLeft textTop
            drawText (drawableLines dData)
            drawCursor dData opts
            restore
            where
                ((Point textLeft textTop), (Point textRight textBottom)) = boundingRect dData

        drawText dLines = do
            setSourceRGBA 0 0.1 0 1
            sequence_ $ map (\(DrawableLine yPos _ pangoShapes) -> do
                    moveTo 0 yPos
                    sequence_ $ map showGlyphString pangoShapes
                ) dLines

        drawCursor dData opts = do
            setLineWidth 1
            setSourceRGBA 0 0 0.3 0.8
            moveTo cursorLeft cursorTop
            lineTo cursorLeft cursorBottom
            stroke
            where
                (Point cursorLeft cursorTop) = cursorPosition dData
                cursorBottom = cursorTop + (fontAscent opts) + (fontDescent opts)
        
        getDrawableData = do
            fancyEditorData <- liftIO $ readMVar fancyEditorDataHolder
            return (drawingData fancyEditorData, drawingOptions fancyEditorData)

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
