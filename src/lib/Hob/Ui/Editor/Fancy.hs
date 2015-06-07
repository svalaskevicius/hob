module Hob.Ui.Editor.Fancy (
    newEditorForText, getActiveEditorWidget, initModule
    ) where

import           Control.Concurrent.MVar             (MVar, modifyMVar_, putMVar, newEmptyMVar,
                                                      readMVar)
import           Control.Monad.Reader
import qualified Control.Monad.State.Lazy            as S
import           Data.Char                           (isPrint, isSpace)
import qualified Data.Foldable                       as F
import qualified Data.Function                       as F
import           Data.Generics
import           Data.Graph
import           Data.List                           (groupBy, sortBy, nubBy, sort)
import           Data.Maybe                          (catMaybes, listToMaybe,
                                                      maybeToList, isJust, mapMaybe)
import  Control.Applicative
import qualified Data.Map as M
import Data.Map (Map)
import           Data.Prizm.Color
import           Data.Prizm.Color.CIE.LCH
import           Data.Text                           (Text, unpack, pack)
import    qualified       Data.Text                as T
import Data.Text.Internal.Search (indices)

import           Data.Traversable                    (traverse)
import           Data.Tree
import qualified Data.Vector                         as V
import           Debug.Trace
import           Filesystem.Path.CurrentOS           (decodeString,
                                                      encodeString, filename)
import           Graphics.Rendering.Cairo
import qualified Graphics.Rendering.Cairo as Cairo
import           Graphics.UI.Gtk                     hiding (Point)
import Graphics.UI.Gtk.General.Enums
import qualified Language.Haskell.Exts.Annotated     as P
import           System.Glib.GObject                 (Quark)
import qualified Graphics.UI.Gtk.General.StyleContext as GtkSc

import           Hob.Context
import           Hob.Control
import           Hob.Context.UiContext
import qualified IPPrint
import qualified Language.Haskell.HsColour           as HsColour
import qualified Language.Haskell.HsColour.Colourise as HsColour
import qualified Language.Haskell.HsColour.Output    as HsColour

import Hob.Ui.Editor.Fancy.Types
import Hob.Ui.Editor.Fancy.Parser
import Hob.Ui.Editor.Fancy.Shapes


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
                emitInternalEditorEvent = \name -> do
                    deferredRunner ctx $ do
                        me <- editorById newEditorId
                        case me of
                            Just e -> emitParametrisedEvent name (holder, e)
                            Nothing -> return(),
                emitEditorEvent = \name -> do
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
    resetSearchPreview = \editor -> liftIO $ do
--        error "not implemented"
        return editor
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
        foundIndices = map (map (flip (,) len) . indices text) $ tLines
        source' = source{textHighlights = foundIndices}
    S.modify $ \ed -> ed{sourceData = source'}
    invokeDraw <- S.gets redraw
    invokeDraw


setEditorModifiedState :: Bool -> S.StateT FancyEditor IO ()
setEditorModifiedState newState = do
    source <- S.gets sourceData
    let source' = source{isModified = newState}
    emitter <- S.gets emitInternalEditorEvent
    liftIO $ emitter "editor.modified.change"
    S.modify $ \ed -> ed{sourceData = source'}


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
                Nothing -> return $ Nothing

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

modifyEditor :: MVar FancyEditor -> S.StateT FancyEditor IO () -> IO ()
modifyEditor fancyEditorDataHolder comp = modifyMVar_ fancyEditorDataHolder $ S.execStateT comp

-- | TODO: invalidate and update partially
updateDrawingData :: PangoContext -> S.StateT FancyEditor IO ()
updateDrawingData pangoContext = do
    source <- S.gets sourceData
    cursor <- S.gets cursorHead
    selection <- S.gets selectionHead
    opts <- S.gets drawingOptions
    dData <- S.gets drawingData
    dData' <- liftIO $ newDrawingData (Just dData) pangoContext source cursor (maybe cursor id selection) opts
    S.modify $ \ed -> ed{drawingData = dData'}

nrOfCharsOnCursorLine :: Int -> S.StateT FancyEditor IO Int
nrOfCharsOnCursorLine yPos = liftM (maybe 0 T.length . listToMaybe . take 1 . drop yPos . textLines) $ S.gets sourceData

clampCursorX :: Int -> Int -> S.StateT FancyEditor IO Int
clampCursorX yPos pos
 | pos < 0 = return 0
 | otherwise = liftM (min pos) $ nrOfCharsOnCursorLine yPos

clampCursorY :: Int -> S.StateT FancyEditor IO Int
clampCursorY pos
 | pos < 0 = return 0
 | otherwise = liftM (min pos) maxCursorY
 where
    maxCursorY = liftM ((flip (-) 1) . length . textLines) $ S.gets sourceData

updateCursorX :: Int -> S.StateT FancyEditor IO ()
updateCursorX delta = do
    CursorHead cx cy _ <- S.gets cursorHead
    cx' <- clampCursorX cy $ cx + delta
    S.modify $ \ed -> ed{cursorHead = CursorHead cx' cy cx'}

moveCursorToTheEndOfTheLine :: S.StateT FancyEditor IO ()
moveCursorToTheEndOfTheLine = do
    source <- S.gets sourceData
    CursorHead _ cy _ <- S.gets cursorHead
    let tLines = textLines source
        cx' = case take 1 . drop cy $ tLines of
            [l] -> T.length l
            _ -> 0
    S.modify $ \ed -> ed{cursorHead = CursorHead cx' cy cx'}

moveCursorToTheStartOfTheLine :: S.StateT FancyEditor IO ()
moveCursorToTheStartOfTheLine = do
    CursorHead _ cy _ <- S.gets cursorHead
    S.modify $ \ed -> ed{cursorHead = CursorHead 0 cy 0}

updateCursorY :: Int -> S.StateT FancyEditor IO ()
updateCursorY delta = do
    CursorHead _ cy cxn <- S.gets cursorHead
    cy' <- clampCursorY $ cy + delta
    cx' <- clampCursorX cy' cxn
    S.modify $ \ed -> ed{cursorHead = CursorHead cx' cy' cxn}

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
        adjustmentClampPage vAdj cursorTop (cursorTop+3*lHeight+30)

deleteSelectedText :: S.StateT FancyEditor IO ()
deleteSelectedText = do
    selection <- S.gets selectionHead
    cursor <- S.gets cursorHead
    maybeDo (deleteTextBetweenCursors cursor) selection
    inEditMode
    where
        deleteTextBetweenCursors c1@(CursorHead x1 y1 _) c2@(CursorHead x2 y2 _) =
            if c1 == c2 then return ()
            else if c1 < c2 then deleteCharacterRange (Point x1 y1) (Point x2 y2)
            else deleteCharacterRange (Point x2 y2) (Point x1 y1)

getSelectedText :: S.StateT FancyEditor IO (Maybe Text)
getSelectedText = do
    selection <- S.gets selectionHead
    cursor <- S.gets cursorHead
    maybe (return Nothing) (getTextBetweenCursors cursor) selection
    where
        getTextBetweenCursors c1@(CursorHead x1 y1 _) c2@(CursorHead x2 y2 _) =
            if c1 == c2 then return Nothing
            else if c1 < c2 then return . Just =<< getTextInRange (Point x1 y1) (Point x2 y2)
            else return . Just =<< getTextInRange (Point x2 y2) (Point x1 y1)

getTextInRange :: PointI -> PointI -> S.StateT FancyEditor IO Text
getTextInRange (Point x1 y1) (Point x2 y2) = do
    source <- S.gets sourceData
    let tLines = textLines source
        postLines = take (y2-y1+1) . drop y1 $ tLines
        firstLine = map (T.drop x1) . take 1 $ postLines
        (midLines, fullLastLine) = splitAt (y2-y1-1) . drop 1 $ postLines
        lastLine = map (T.take x2) fullLastLine
        multiline = y2 > y1
        singleLine = head $ (map (T.take (x2-x1)) firstLine) ++ [T.empty]
        multipleLines = (T.unlines $ firstLine ++ midLines) `T.append` (head $ lastLine ++ [T.empty])
    return $ if multiline then multipleLines else singleLine

putSelectedTextToClipboard :: S.StateT FancyEditor IO ()
putSelectedTextToClipboard = do
    selectedText <- getSelectedText
    maybeDo (liftIO . putTextToClipboard) selectedText
    where
        putTextToClipboard text = do
            cb <- clipboardGet selectionClipboard
            clipboardSetText cb text

textSnippetToLines :: Text -> [Text]
textSnippetToLines text = (T.lines text) ++ (if T.null text then [] else if T.last text == '\n' then [T.empty] else [])

insertEditorText :: Text -> S.StateT FancyEditor IO ()
insertEditorText text = do
    source <- S.gets sourceData
    CursorHead cx cy _ <- S.gets cursorHead
    let newLines = textSnippetToLines text
        events = [InsertText (Point cx cy) newLines]
    source' <- liftIO $ newSourceData (Left (source, events))
    let (Point cx' cy') = foldr adjustPointByEvent (Point cx cy) events
    S.modify $ \ed -> ed{sourceData = source', cursorHead = CursorHead cx' cy' cx'}
    setEditorModifiedState True

insertEditorChar :: Char -> S.StateT FancyEditor IO ()
insertEditorChar c = do
    source <- S.gets sourceData
    CursorHead cx cy _ <- S.gets cursorHead
    let events = [InsertText (Point cx cy) (textSnippetToLines (T.singleton c))]
    source' <- liftIO $ newSourceData (Left (source, events))
    let (Point cx' cy') = foldr adjustPointByEvent (Point cx cy) events
    S.modify $ \ed -> ed{sourceData = source', cursorHead = CursorHead cx' cy' cx'}
    setEditorModifiedState True

deleteCharacterRange :: Point Int -> Point Int -> S.StateT FancyEditor IO ()
deleteCharacterRange startPoint@(Point sx sy) endPoint = do
    source <- S.gets sourceData
    deletedText <- getTextInRange startPoint endPoint
    let deletedLines = textSnippetToLines deletedText
    source' <- liftIO $ newSourceData (Left (source, [DeleteText startPoint deletedLines]))
    S.modify $ \ed -> ed{sourceData = source', cursorHead = CursorHead sx sy sx}
    setEditorModifiedState True


deleteCharactersFromCursor :: Int -> S.StateT FancyEditor IO ()
deleteCharactersFromCursor amount = do
    source <- S.gets sourceData
    CursorHead cx cy _ <- S.gets cursorHead
    let tLines = textLines source
        (preLines, postLines) = splitAt cy tLines
        (startx, endx) = if amount > 0 then (cx, cx+amount) else (cx+amount, cx)
        (startx', adjsy) = adjustNegativePosByLengths (cx : (map T.length . reverse $ preLines)) startx
        starty' = max 0 $ cy + adjsy
        (endx', adjey) = adjustPositivePosByLengths (map T.length $ postLines) endx
        endy' = cy + adjey
    deleteCharacterRange (Point startx' starty') (Point endx' endy')
    where
        adjustNegativePosByLengths [] startx
         | startx < 0 = (0, 0)
         | otherwise = (startx, 0)
        adjustNegativePosByLengths (tl:tls) startx
         | startx < 0 = let (startx', adjusty) = adjustNegativePosByLengths tls backpos'
                        in (startx', adjusty-1)
         | otherwise = (startx, 0)
            where
                backpos = startx+tl+1
                endOfPrevLine = head $ tls++[0]
                backpos' = if backpos == 0 then endOfPrevLine else backpos
        adjustPositivePosByLengths [] endx = (endx, 0)
        adjustPositivePosByLengths (tl:tls) endx
         | endx > tl = let (startx', adjusty) = adjustPositivePosByLengths tls (endx-tl-1)
                        in (startx', adjusty+1)
         | otherwise = (endx, 0)


insertNewLine :: S.StateT FancyEditor IO ()
insertNewLine = do
    source <- S.gets sourceData
    CursorHead cx cy _ <- S.gets cursorHead
    source' <- liftIO $ newSourceData (Left (source, [InsertText (Point cx cy) (textSnippetToLines (T.singleton '\n'))])) -- tLines'
    S.modify $ \ed -> ed{sourceData = source', cursorHead = CursorHead 0 (cy+1) 0}
    setEditorModifiedState True

inSelectionMode :: S.StateT FancyEditor IO ()
inSelectionMode = do
    selection <- S.gets selectionHead
    if not $ isJust selection then do
        cursor <- S.gets cursorHead
        S.modify $ \ed -> ed{selectionHead = Just cursor}
    else return ()

inEditMode :: S.StateT FancyEditor IO ()
inEditMode = do
    selection <- S.gets selectionHead
    if isJust selection then do
        S.modify $ \ed -> ed{selectionHead = Nothing}
    else return ()

deleteSelectionOrInvokeCommand :: (S.StateT FancyEditor IO ()) -> S.StateT FancyEditor IO ()
deleteSelectionOrInvokeCommand cmd = do
    selection <- S.gets selectionHead
    if isJust selection then do
        deleteSelectedText
    else cmd

deleteBackwards :: S.StateT FancyEditor IO ()
deleteBackwards = deleteSelectionOrInvokeCommand $ deleteCharactersFromCursor (-1)

deleteForwards :: S.StateT FancyEditor IO ()
deleteForwards = deleteSelectionOrInvokeCommand $ deleteCharactersFromCursor 1

undoLast :: S.StateT FancyEditor IO ()
undoLast = do
    source <- S.gets sourceData
    let eventHistory = history source
    if null eventHistory then return ()
    else do
        let ([currentEvent], restOfTheEvents) = splitAt 1 eventHistory
        let reversedEvents = reverseEditorEvent currentEvent
        source' <- liftIO $ newSourceData (Left (source, reversedEvents))
        let source'' = source'{
                           history = restOfTheEvents,
                           undoneHistory = currentEvent : (undoneHistory source')
                       }
        S.modify $ \ed -> ed{sourceData = source''}
        adjustCursorsByEvents reversedEvents
        setEditorModifiedState True

redoLast :: S.StateT FancyEditor IO ()
redoLast = do
    source <- S.gets sourceData
    let eventHistory = undoneHistory source
    if null eventHistory then return ()
    else do
        let (currentEvent, restOfTheEvents) = splitAt 1 eventHistory
        source' <- liftIO $ newSourceData (Left (source, currentEvent))
        let source'' = source'{
                           undoneHistory = restOfTheEvents
                       }
        S.modify $ \ed -> ed{sourceData = source''}
        adjustCursorsByEvents currentEvent
        setEditorModifiedState True

adjustCursorsByEvents :: [SourceChangeEvent] -> S.StateT FancyEditor IO ()
adjustCursorsByEvents events = do
    selection <- S.gets selectionHead
    cursor <- S.gets cursorHead
    S.modify $ \ed -> ed {
                          selectionHead = fmap adjustCursor selection,
                          cursorHead = adjustCursor cursor
                      }
    where
        adjustCursor (CursorHead cx cy _) = CursorHead cx' cy' cx'
            where
                (Point cx' cy') = foldr adjustPointByEvent (Point cx cy) events

invokeEditorCommand :: (ScrolledWindowClass s, WidgetClass w) => MVar FancyEditor -> w -> PangoContext -> s -> S.StateT FancyEditor IO () -> IO ()
invokeEditorCommand fancyEditorDataHolder editorWidget pangoContext scrolledWindow cmd  = do
    modifyEditor fancyEditorDataHolder $ do
        cmd
        scrollEditorToCursor scrolledWindow
        invokeDraw <- S.gets redraw
        invokeDraw

keyEventHandler :: (ScrolledWindowClass s, WidgetClass w) => MVar FancyEditor -> w -> PangoContext -> s -> EventM EKey Bool
keyEventHandler fancyEditorDataHolder editorWidget pangoContext scrolledWindow = do
    modifiers <- eventModifier
    keyValue <- eventKeyVal
    let printableChar = (\c -> if isPrint c then Just c else Nothing) =<< keyToChar keyValue
    -- TODO: widgetQueueDraw should only redraw previous and next cursor regions
    -- TODO: handle text selection, cut, copy, paste
    let invokeEditorCmd cmd  = (liftIO $ invokeEditorCommand fancyEditorDataHolder editorWidget pangoContext scrolledWindow cmd) >> return True

    let insertTextFromClipboard = do
            cb <- clipboardGet selectionClipboard
            clipboardRequestText cb onTextReceived
            where
                onTextReceived = maybeDo (\text -> (invokeEditorCmd $ insertEditorText text) >> return())

    let getLinesInOnePage = do
            widgetHeight <- liftIO $ widgetGetAllocatedHeight scrolledWindow
            opts <- S.gets drawingOptions
            return $ floor $ (fromIntegral widgetHeight) / (lineHeight opts)

    let key = unpack $ keyName keyValue
        navigationCmd = case key of
            "Right" -> Just $ updateCursorX 1
            "Left" -> Just $ updateCursorX (-1)
            "Up" -> Just $ updateCursorY (-1)
            "Down" -> Just $ updateCursorY 1
            "Page_Up" -> Just $ do
                linesToJump <- getLinesInOnePage
                updateCursorY (-linesToJump)
            "Page_Down" -> Just $ do
                linesToJump <- getLinesInOnePage
                updateCursorY linesToJump
            "Home" -> Just $ moveCursorToTheStartOfTheLine
            "End" -> Just $ moveCursorToTheEndOfTheLine
            _ -> Nothing

    case navigationCmd of
        Just cmd -> case modifiers of
            [Shift] -> invokeEditorCmd $ inSelectionMode >> cmd
            [] -> invokeEditorCmd $ inEditMode >> cmd
            _ -> return False
        Nothing ->  case (modifiers, key) of
            ([Control], "c") -> invokeEditorCmd putSelectedTextToClipboard
            ([Control], "x") -> invokeEditorCmd (putSelectedTextToClipboard >> deleteSelectedText)
            ([Control], "v") -> (invokeEditorCmd deleteSelectedText) >> (liftIO $ insertTextFromClipboard) >> return True
            ([Control], "z") -> invokeEditorCmd undoLast
            ([Shift, Control], "Z") -> invokeEditorCmd redoLast
            ([], "BackSpace") -> invokeEditorCmd $ deleteBackwards
            ([], "Delete") -> invokeEditorCmd $ deleteForwards
            ([], "Return") -> invokeEditorCmd $ (deleteSelectedText >> insertNewLine)
            _ -> if (null modifiers) || (modifiers == [Shift]) then
                    maybe
                        ((liftIO $ debugPrint (modifiers, unpack $ keyName keyValue)) >> return False)
                        (\c -> invokeEditorCmd $ (deleteSelectedText >> insertEditorChar c))
                        printableChar
                 else ((liftIO $ debugPrint (modifiers, unpack $ keyName keyValue)) >> return False)


data ComparisonResult1DRange = Before | Inside | After deriving (Eq, Show)

drawEditor :: WidgetClass w => MVar FancyEditor -> w -> Render ()
drawEditor fancyEditorDataHolder editorWidget = do
        drawData <- getDrawableData
        updateWidgetSize drawData
        drawBackground
        drawContents drawData
    where
        drawBackground = do
            setSourceRGB 0.86 0.85 0.8
            paint

        updateWidgetSize (dData, _) = liftIO $ widgetSetSizeRequest editorWidget (ceiling textRight) (ceiling textBottom)
            where
                (_, Point textRight textBottom) = boundingRect dData

        drawPaths cmds paths = traverse (traverse $ drawPath cmds) paths >> return()

        drawPath _ [] = return()
        drawPath pathCmds ((Point px py):ps) = do
            moveTo px py
            mapM_ (\(Point lx ly) -> lineTo lx ly) ps
            closePath
            pathCmds

        drawContents (dData, opts) = do
            save
            translate textLeft textTop
            rect <- getClipRectangle
            drawPaths bgBackgroundPainter $ bgPathsToDraw rect
            drawPath selectionBackgroundPainter $ selectionContour dData
            drawErrorPointer dData opts
            drawText (linesToDraw rect) (colourGroupToRgb dData)
            drawCursor dData opts
            restore
            liftIO $ (reportError opts) $ sourceErrorMessage dData

            where
                bgBackgroundPainter = do
                    setSourceRGBA 0.3 0.3 0.4 0.05
                    strokePreserve
                    setSourceRGBA 0.69 0.65 0.5 0.15
                    fill
                selectionBackgroundPainter = do
                    setSourceRGBA 1 1 0.5 0.7
                    strokePreserve
                    setSourceRGBA 1 1 0.7 0.9
                    fill

                linesToDraw Nothing = drawableLines dData
                linesToDraw (Just (Rectangle _ ry _ rh)) = filterLines $ drawableLines dData
                    where
                        filterLines = takeWhile lineBeforeRectEnd . dropWhile lineBeforeRect
                            where
                                lineBeforeRect (DrawableLine ly _ _) = (ly + (fontDescent opts)) < fromIntegral ry
                                lineBeforeRectEnd (DrawableLine ly _ _) = (ly - (fontAscent opts)) < fromIntegral (ry + rh)
                (Point textLeft textTop, _) = boundingRect dData
                bgPathsToDraw Nothing = backgroundPaths dData
                bgPathsToDraw (Just rect) = filterPaths rect $ backgroundPaths dData
                filterPaths :: Rectangle -> (Forest [PointD]) -> Forest [PointD]
                filterPaths rect@(Rectangle rx ry rw rh) = map (\n->Node (rootLabel n) (filterPaths rect $ subForest n) ) . filter (doesPathIntersectRectangle . rootLabel)
                    where
                        doesPathIntersectRectangle = checkRangeResults . map pointRelToRect
                            where
                                pointRelToRect (Point px py) = (comparePos1D px rx (rx+rw), comparePos1D py ry (ry+rh))
                                comparePos1D :: Double -> Int -> Int -> ComparisonResult1DRange
                                comparePos1D a b1 b2
                                    | a < (fromIntegral b1) = Before
                                    | (a >= (fromIntegral b1)) && (a <= (fromIntegral b2)) = Inside
                                    | otherwise = After
                                checkRangeResults ((Inside, Inside):_) = True
                                checkRangeResults [] = False
                                checkRangeResults [_] = False
                                checkRangeResults ((rx1, ry1):(rx2, ry2):rs) = checkRangeResults ((rx1 `combinePoints` rx2, ry1 `combinePoints` ry2):rs)
                                    where
                                        combinePoints Before Before = Before
                                        combinePoints After After = After
                                        combinePoints _ _ = Inside

        drawText dLines colourRgb = do
            setSourceRGBA 0 0.1 0 1
            mapM_ (\(DrawableLine yPos _ pangoShapes) -> do
                    moveTo 0 yPos
                    mapM_ drawTextPiece pangoShapes
                ) dLines
            where
                drawTextPiece (w, c, s) = do
            -- TODO: filter only pieces in clip rect
                    setTextColour c
                    showGlyphString s
                    relMoveTo w 0
                setTextColour DefaultColourGroup = setSourceRGB 0 0 0
                setTextColour (ColourGroup c) = do
                    let (r, g, b) = colourRgb V.! c
                    setSourceRGB r g b

        drawErrorPointer dData opts = case errorPosition dData of
                                          Just (Point errorLeft errorTop) -> do
                                                                              setSourceRGBA 1 0.3 0.3 0.8
                                                                              Cairo.rectangle errorLeft errorTop lh lh
                                                                              fill
                                          Nothing -> return ()
            where
                lh = fontAscent opts + fontDescent opts

        drawCursor dData opts = do
            setLineWidth 1
            setSourceRGBA 0 0 0.3 0.8
            moveTo cursorLeft cursorTop
            lineTo cursorLeft cursorBottom
            stroke
            where
                (Point cursorLeft cursorTop) = cursorPosition dData
                cursorBottom = cursorTop + fontAscent opts + fontDescent opts

        getDrawableData = do
            fancyEditorData <- liftIO $ readMVar fancyEditorDataHolder
            return (drawingData fancyEditorData, drawingOptions fancyEditorData)

getActiveEditor :: Context -> IO (Maybe DrawingArea)
getActiveEditor = maybe (return Nothing) getEditorFromNotebookTab <=< getActiveEditorTab

getActiveEditorWidget :: Context -> IO (Maybe Widget)
getActiveEditorWidget = maybe (return Nothing) getEditorWidgetFromNotebookTab <=< getActiveEditorTab

getEditorWidgetFromNotebookTab :: Widget -> IO (Maybe Widget)
getEditorWidgetFromNotebookTab = recurseInBin
    where
        binChild widget = if widget `isA` gTypeBin then do
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
        asDrawingArea widget = if widget `isA` gTypeDrawingArea then do
                                   return $ Just $ castToDrawingArea widget
                               else do
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
