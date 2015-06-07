module Hob.Ui.Editor.Fancy.Commands (
  keyEventHandler,
  modifyEditor,
  newSourceDataFromText,
  setEditorModifiedState,
    ) where

import           Control.Concurrent.MVar    (MVar, modifyMVar_)
import           Control.Monad.Reader
import qualified Control.Monad.State.Lazy   as S
import           Data.Char                  (isPrint)
import           Data.Maybe                 (isJust, listToMaybe)
import           Data.Text                  (Text, unpack)
import qualified Data.Text                  as T

import           Graphics.UI.Gtk            hiding (Point)

import           Hob.Control

import           Hob.Ui.Editor.Fancy.Parser
import           Hob.Ui.Editor.Fancy.Types


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
    maxCursorY = liftM (flip (-) 1 . length . textLines) $ S.gets sourceData

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
        deleteTextBetweenCursors c1@(CursorHead x1 y1 _) c2@(CursorHead x2 y2 _)
          | c1 == c2 = return ()
          | c1 < c2 = deleteCharacterRange (Point x1 y1) (Point x2 y2)
          | otherwise = deleteCharacterRange (Point x2 y2) (Point x1 y1)

getSelectedText :: S.StateT FancyEditor IO (Maybe Text)
getSelectedText = do
    selection <- S.gets selectionHead
    cursor <- S.gets cursorHead
    maybe (return Nothing) (getTextBetweenCursors cursor) selection
    where
        getTextBetweenCursors c1@(CursorHead x1 y1 _) c2@(CursorHead x2 y2 _)
          | c1 == c2 = return Nothing
          | c1 < c2 = return . Just =<< getTextInRange (Point x1 y1) (Point x2 y2)
          | otherwise = return . Just =<< getTextInRange (Point x2 y2) (Point x1 y1)

getTextInRange :: PointI -> PointI -> S.StateT FancyEditor IO Text
getTextInRange (Point x1 y1) (Point x2 y2) = do
    source <- S.gets sourceData
    let tLines = textLines source
        postLines = take (y2-y1+1) . drop y1 $ tLines
        firstLine = map (T.drop x1) . take 1 $ postLines
        (midLines, fullLastLine) = splitAt (y2-y1-1) . drop 1 $ postLines
        lastLine = map (T.take x2) fullLastLine
        multiline = y2 > y1
        singleLine = head $ map (T.take (x2-x1)) firstLine ++ [T.empty]
        multipleLines = T.unlines (firstLine ++ midLines) `T.append` head (lastLine ++ [T.empty])
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
textSnippetToLines text = T.lines text ++ (if T.null text then [] else [T.empty | T.last text == '\n'])

insertEditorText :: Text -> S.StateT FancyEditor IO ()
insertEditorText text = do
    source <- S.gets sourceData
    CursorHead cx cy _ <- S.gets cursorHead
    let newLines = textSnippetToLines text
        events = [InsertText (Point cx cy) newLines]
    updateSourceByEvents source events

insertEditorChar :: Char -> S.StateT FancyEditor IO ()
insertEditorChar c = do
    source <- S.gets sourceData
    CursorHead cx cy _ <- S.gets cursorHead
    let events = [InsertText (Point cx cy) (textSnippetToLines (T.singleton c))]
    updateSourceByEvents source events

updateSourceByEvents :: SourceData -> [SourceChangeEvent] -> S.StateT FancyEditor IO ()
updateSourceByEvents source events = do
    CursorHead cx cy _ <- S.gets cursorHead
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
        (endx', adjey) = adjustPositivePosByLengths (map T.length postLines) endx
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
    unless (isJust selection) $ do
        cursor <- S.gets cursorHead
        S.modify $ \ed -> ed{selectionHead = Just cursor}

inEditMode :: S.StateT FancyEditor IO ()
inEditMode = do
    selection <- S.gets selectionHead
    when (isJust selection) $ S.modify $ \ed -> ed{selectionHead = Nothing}

deleteSelectionOrInvokeCommand :: S.StateT FancyEditor IO () -> S.StateT FancyEditor IO ()
deleteSelectionOrInvokeCommand cmd = do
    selection <- S.gets selectionHead
    if isJust selection then deleteSelectedText
    else cmd

deleteBackwards :: S.StateT FancyEditor IO ()
deleteBackwards = deleteSelectionOrInvokeCommand $ deleteCharactersFromCursor (-1)

deleteForwards :: S.StateT FancyEditor IO ()
deleteForwards = deleteSelectionOrInvokeCommand $ deleteCharactersFromCursor 1

undoLast :: S.StateT FancyEditor IO ()
undoLast = do
    source <- S.gets sourceData
    let eventHistory = history source
    unless (null eventHistory) $ do
        let ([currentEvent], restOfTheEvents) = splitAt 1 eventHistory
        let reversedEvents = reverseEditorEvent currentEvent
        source' <- liftIO $ newSourceData (Left (source, reversedEvents))
        let source'' = source'{
                           history = restOfTheEvents,
                           undoneHistory = currentEvent : undoneHistory source'
                       }
        S.modify $ \ed -> ed{sourceData = source''}
        adjustCursorsByEvents reversedEvents
        setEditorModifiedState True

redoLast :: S.StateT FancyEditor IO ()
redoLast = do
    source <- S.gets sourceData
    let eventHistory = undoneHistory source
    unless (null eventHistory) $ do
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
invokeEditorCommand fancyEditorDataHolder _ _ scrolledWindow cmd  =
    modifyEditor fancyEditorDataHolder $ do
        cmd
        scrollEditorToCursor scrolledWindow
        join $ S.gets redraw

keyEventHandler :: (ScrolledWindowClass s, WidgetClass w) => MVar FancyEditor -> w -> PangoContext -> s -> EventM EKey Bool
keyEventHandler fancyEditorDataHolder editorWidget pangoContext scrolledWindow = do
    modifiers <- eventModifier
    keyValue <- eventKeyVal
    let printableChar = (\c -> if isPrint c then Just c else Nothing) =<< keyToChar keyValue
    -- TODO: widgetQueueDraw should only redraw previous and next cursor regions
    -- TODO: handle text selection, cut, copy, paste
    let invokeEditorCmd cmd  = liftIO (invokeEditorCommand fancyEditorDataHolder editorWidget pangoContext scrolledWindow cmd) >> return True

    let insertTextFromClipboard = do
            cb <- clipboardGet selectionClipboard
            clipboardRequestText cb onTextReceived
            where
                onTextReceived = maybeDo (void . invokeEditorCmd . insertEditorText)

    let getLinesInOnePage = do
            widgetHeight <- liftIO $ widgetGetAllocatedHeight scrolledWindow
            opts <- S.gets drawingOptions
            return $ floor $ fromIntegral widgetHeight / lineHeight opts

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
            "Home" -> Just moveCursorToTheStartOfTheLine
            "End" -> Just moveCursorToTheEndOfTheLine
            _ -> Nothing

    case navigationCmd of
        Just cmd -> case modifiers of
            [Shift] -> invokeEditorCmd $ inSelectionMode >> cmd
            [] -> invokeEditorCmd $ inEditMode >> cmd
            _ -> return False
        Nothing ->  case (modifiers, key) of
            ([Control], "c") -> invokeEditorCmd putSelectedTextToClipboard
            ([Control], "x") -> invokeEditorCmd (putSelectedTextToClipboard >> deleteSelectedText)
            ([Control], "v") -> invokeEditorCmd deleteSelectedText >> liftIO insertTextFromClipboard >> return True
            ([Control], "z") -> invokeEditorCmd undoLast
            ([Shift, Control], "Z") -> invokeEditorCmd redoLast
            ([], "BackSpace") -> invokeEditorCmd deleteBackwards
            ([], "Delete") -> invokeEditorCmd deleteForwards
            ([], "Return") -> invokeEditorCmd (deleteSelectedText >> insertNewLine)
            _ -> if null modifiers || (modifiers == [Shift]) then
                    maybe
                        (liftIO $ debugPrint (modifiers, unpack $ keyName keyValue) >> return False)
                        (\c -> invokeEditorCmd (deleteSelectedText >> insertEditorChar c))
                        printableChar
                 else liftIO $ debugPrint (modifiers, unpack $ keyName keyValue) >> return False


setEditorModifiedState :: Bool -> S.StateT FancyEditor IO ()
setEditorModifiedState newState = do
    source <- S.gets sourceData
    let source' = source{isModified = newState}
    emitter <- S.gets emitInternalEditorEvent
    liftIO $ emitter "editor.modified.change"
    S.modify $ \ed -> ed{sourceData = source'}

modifyEditor :: MVar FancyEditor -> S.StateT FancyEditor IO () -> IO ()
modifyEditor fancyEditorDataHolder comp = modifyMVar_ fancyEditorDataHolder $ S.execStateT comp
