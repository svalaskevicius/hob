module Hob.Ui.Editor.Fancy (
    newEditorForText
    ) where

import           Control.Concurrent.MVar   (MVar, modifyMVar_, newMVar,
                                            readMVar)
import           Control.Monad.Reader
import qualified Control.Monad.State.Lazy  as S
import           Data.Char                 (isPrint, isSpace)
import           Data.Maybe                (listToMaybe, mapMaybe)
import           Data.Text                 (Text, unpack)
import           Filesystem.Path.CurrentOS (decodeString, encodeString,
                                            filename)
import           Graphics.Rendering.Cairo
import           Graphics.UI.Gtk           hiding (Point)
import           System.Glib.GObject       (Quark)
import qualified Language.Haskell.Exts.Annotated as P
import Data.Tree
import Data.Traversable (traverse)
import qualified Data.Foldable as F
import Data.Generics

import           Hob.Context
import           Hob.Context.UiContext

data Block a = Block (Point a) (Point a) deriving Show
type BlockD = Block Double
type BlockI = Block Int
type Blocks a = Forest (Block a)

data VariableDependency = VariableDependency (P.Name P.SrcSpanInfo) [P.Name P.SrcSpanInfo] deriving Show

data ColourGroup = DefaultColourGroup | ColourGroup Int

data ColouredRange = ColouredRange 
                        Int -- ^ pos
                        Int -- ^ length
                        ColourGroup -- ^ colour group

data SourceData = SourceData {
    isModified   :: Bool,
    textLines    :: [String],
    parseResult  :: P.ParseResult (P.Module P.SrcSpanInfo, [P.Comment]),
    sourceBlocks :: Blocks Int,
    varDeps      :: [VariableDependency]
}

-- | Point x y
data Point a = Point a a deriving Show
type PointD = Point Double
type PointI = Point Int

--data DrawableG
-- | DrawableLine (y position) (x position of each char) [(width of the glyph, colour group, glyph to draw)]
data DrawableLine = DrawableLine Double [Double] [(Double, ColourGroup, GlyphItem)]

data EditorDrawingData = EditorDrawingData {
    drawableLines   :: [DrawableLine],
    boundingRect    :: (PointD, PointD),
    cursorPosition  :: PointD,
    backgroundPaths :: Forest [PointD]
}

data EditorDrawingOptions = EditorDrawingOptions {
    lineHeight  :: Double,
    fontAscent  :: Double,
    fontDescent :: Double
}

data FancyEditor = FancyEditor {
    sourceData     :: SourceData,
    cursorPos      :: (Int, Int, Int), -- ^ X, Y, X_{navigation}
    drawingOptions :: EditorDrawingOptions,
    drawingData    :: EditorDrawingData
}

findVars :: P.Exp P.SrcSpanInfo -> [P.Name P.SrcSpanInfo]
findVars (P.Var _ (P.UnQual _ name)) = [name]
findVars _ = []

findNames :: P.Name P.SrcSpanInfo -> [P.Name P.SrcSpanInfo]
findNames name = [name]

findNamesInPatterns :: [P.Pat P.SrcSpanInfo] -> [P.Name P.SrcSpanInfo]
findNamesInPatterns = concatMap (findElements findNames)

findFuncs :: P.Match P.SrcSpanInfo -> [(P.Name P.SrcSpanInfo, [P.Name P.SrcSpanInfo], [P.Name P.SrcSpanInfo])]
findFuncs (P.Match _ name pat rhs _) = [(name, findNamesInPatterns pat, findElements findVars rhs)]
findFuncs (P.InfixMatch _ pat1 name pat2 rhs _) = [(name, (findElements findNames pat1) ++ (findNamesInPatterns pat2), findElements findVars rhs)]

findElements :: (Data a, Typeable b) => (b -> [c]) -> a -> [c]
findElements query a = ([] `mkQ` query $ a) ++ (concat $ gmapQ (findElements query) a)

findVariableDependencies :: (Data l) => [P.Decl l] -> [VariableDependency]
findVariableDependencies decls = funcUsages ++ varUsages
    where
        varElements = findElements findFuncs decls
        funcs = map (\(f, _, _)->f) varElements
        funcUsages = map (\f->VariableDependency f (concatMap (\(_, _, usages)-> filter (f P.=~=) usages) varElements)) funcs
        varUsages = concatMap (\(_, vars, usages) -> map (\v->VariableDependency v (filter (v P.=~=) usages)) vars) varElements

-- TODO: vector for lines?
newSourceData :: Text -> IO SourceData
newSourceData text = return sd
    where
        parseMode = P.defaultParseMode
        textAsString = unpack $ text
        parsedModule = P.parseFileContentsWithComments parseMode textAsString
        declarations (P.ParseOk (P.Module _ _ _ _ decl, _)) = decl
        declarations (P.ParseFailed _ _) = []
        declarations _ = error "unsupported parse result"
        collectSourceBlocks = map (
                (\s -> Block 
                        (Point (P.srcSpanStartColumn s - 1) (P.srcSpanStartLine s - 1))
                        (Point (P.srcSpanEndColumn s - 1) (P.srcSpanEndLine s - 1))
                )
            ) . concatMap (F.foldr (\a s->(s++[P.srcInfoSpan a])) [])
        variableDeps = findVariableDependencies . declarations $ parsedModule
        sd = SourceData {
            isModified = False,
            textLines = lines textAsString,
            parseResult = parsedModule,
            sourceBlocks = map (\b -> Node b []) . collectSourceBlocks $ declarations parsedModule,
            varDeps = variableDeps
        }

newDrawingData :: PangoContext -> SourceData -> (Int, Int, Int) -> EditorDrawingOptions -> IO EditorDrawingData
newDrawingData pangoContext source (cursorCharNr, cursorLineNr, _) opts = do
    lineData <- newDrawableLineData pangoContext source opts
    let cursorP = sourcePointToDrawingPoint (drawableLineWidths lineData) (Point cursorCharNr cursorLineNr) opts
    return $ ed lineData cursorP
    where
        sourceCoordsToDrawing lineData p = sourcePointToDrawingPoint (drawableLineWidths lineData) p opts
        convertBlocks lineData = fmap . fmap $ convertBlock
            where
                convertBlock (Block tl br) = if multiline then [dTopLeft, dTopRight, dBottomRight, dBottomLeft, dTopLeftSecondLine, dBottomLeftFirstLine]
                                             else [dTopLeft, dTopRight, dBottomRight, dBottomLeftFirstLine]
                    where
                        multiline = let (Point _ sy1) = tl
                                        (Point _ sy2) = br
                                    in sy2 > sy1
                        dTopLeft = sourceCoordsToDrawing lineData tl
                        dBottomRight = movePointToMaxRightOfTheRange tl br . movePointToLineBottom $ sourceCoordsToDrawing lineData br
                        dTopRight = let (Point _ tly) = dTopLeft
                                        (Point brx _) = dBottomRight
                                    in Point brx tly
                        dBottomLeftFirstLine = let (Point tlx tly) = dTopLeft
                                               in Point tlx (tly + lineHeight opts)
                        dBottomLeft = let (Point tlx _) = dTopLeft
                                          (Point _ bry) = dBottomRight
                                          (Point _ sy1) = tl
                                          (Point _ sy2) = br
                                      in Point (min tlx $ leftMostCharBetween (sy1+1) sy2) bry
                        dTopLeftSecondLine = let (Point _ tly) = dTopLeft
                                                 (Point blx _) = dBottomLeft
                                             in Point blx (tly + lineHeight opts)
                leftMostCharBetween :: Int -> Int -> Double
                leftMostCharBetween l1 l2 = maximum . (0:) $ lineWhiteSpacePrefixWidths
                    where
                        lineTextRange = take (l2-l1) . drop (l1) $ (textLines source)
                        lineWhiteSpacePrefixes :: [String]
                        lineWhiteSpacePrefixes = map (takeWhile isSpace) lineTextRange
                        lineWhiteSpacePrefixLengths :: [Int]
                        lineWhiteSpacePrefixLengths = map length lineWhiteSpacePrefixes
                        lineWhiteSpacePrefixWidths :: [Double]
                        lineWhiteSpacePrefixWidths = zipWith (\l w -> sum $ take l w) lineWhiteSpacePrefixLengths lineWidthRange 
                        lineWidthRange :: [[Double]]
                        lineWidthRange = take (l2-l1) . drop (l1) $ (drawableLineWidths lineData)
                movePointToLineBottom (Point px py) = Point px (py + (lineHeight opts))
                movePointToMaxRightOfTheRange (Point _ l1) (Point _ l2) (Point px py) = Point (max px maxRight) py
                    where
                        maxRight = maximum . (0:) . map sum $ lineWidthRange
                            where
                                lineWidthRange = take (l2-l1) . drop (l1) $ (drawableLineWidths lineData)
        ed lineData cursorP = EditorDrawingData {
            drawableLines = lineData,
            boundingRect = getBoundingRect opts lineData,
            cursorPosition = cursorP,
            backgroundPaths = convertBlocks lineData (sourceBlocks source)
        }

newDrawingOptions :: PangoContext -> IO EditorDrawingOptions
newDrawingOptions pangoContext = do
    fontDescription <- contextGetFontDescription pangoContext
    fontSizeInfo <- contextGetMetrics pangoContext fontDescription emptyLanguage
    let a = ascent fontSizeInfo
        d = descent fontSizeInfo
    return EditorDrawingOptions {
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
    return FancyEditor {
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
            widgetShowAll scrolledWindow
            tabNr <- notebookAppendPage targetNotebook scrolledWindow title
            notebookSetCurrentPage targetNotebook tabNr
            notebookSetShowTabs targetNotebook True
            return scrolledWindow

        newPangoContext = do
            pangoContext <- cairoCreateContext Nothing
            fontDescription <- fontDescriptionNew
            fontDescriptionSetFamily fontDescription "Ubuntu"
            fontDescriptionSetSize fontDescription 18
            contextSetFontDescription pangoContext fontDescription
            return pangoContext

        createNewEditor ctx = do
            editorWidget <- newEditorWidget =<< idGenerator ctx
            scrolledWindow <- newEditorScrolls editorWidget
            pangoContext <- newPangoContext

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
nrOfCharsOnCursorLine yPos = liftM (maybe 0 length . listToMaybe . take 1 . drop yPos . textLines) $ S.gets sourceData

clampCursorX :: Int -> Int -> S.StateT FancyEditor IO Int
clampCursorX yPos pos
 | pos < 0 = return 0
 | otherwise = liftM (min pos) $ nrOfCharsOnCursorLine yPos

clampCursorY :: Int -> S.StateT FancyEditor IO Int
clampCursorY pos
 | pos < 0 = return 0
 | otherwise = liftM (min pos) nrOfLines
 where
    nrOfLines = liftM (length . textLines) $ S.gets sourceData

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

sourcePointToDrawingPoint :: [[Double]] -> PointI -> EditorDrawingOptions -> PointD
sourcePointToDrawingPoint lineWidths (Point cx cy) opts = Point cursorLeft cursorTop
    where
        cursorTop = fromIntegral cy * lHeight - fAscent
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
        tLines' = preLines ++ changedLine ++ drop 1 postLines
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

-- | [(String, [(Int, Int)]) = [(line, [ColouredRange])]
getLineShapesWithWidths :: PangoContext -> [(String, [ColouredRange])] -> IO ([[(Double, ColourGroup, GlyphItem)]], [[Double]])
getLineShapesWithWidths pangoContext linesToDraw = do
    pangoLineShapes <- mapM (\(line, pieces) -> do
            pangoItems <- mapM (\partLine -> pangoItemize pangoContext partLine []) (listToPieces (relativePiecePositions pieces) line)
            mapM pangoShape (concat pangoItems)
        ) breakPointedLines

    extents <- mapM (mapM glyphItemExtents) pangoLineShapes
    lineWordWidths <- mapM (mapM (`glyphItemGetLogicalWidths` Nothing)) pangoLineShapes
    let sizedLineShapes = zipWith3 (\le lc ls -> zipWith3 (\e c s -> (glyphWidthFromExtent e, c, s)) le lc ls) extents ((map (coloursOfThePieces.snd) breakPointedLines)) pangoLineShapes
    return (sizedLineShapes, map concat lineWordWidths)
    where
        glyphWidthFromExtent (_, (PangoRectangle _ _ w _)) = w
        listToPieces :: [Int] -> [a] -> [[a]]
        listToPieces [] l = [l]
        listToPieces (p:ps) l = let (l1, lRest) = splitAt p l
                                in l1:listToPieces ps lRest
        breakPointedLines = map (\(line, ranges) -> (line, colouredRangesToBreakPoints ranges)) linesToDraw
        relativePiecePositions pieces = map fst pieces
        coloursOfThePieces pieces = map snd pieces
        colouredRangesToBreakPoints [] = []
        colouredRangesToBreakPoints ((ColouredRange p1 l1 c1):rs1) = removeZeroLengths $ [(0, DefaultColourGroup), (p1, c1), (l1, DefaultColourGroup)] ++ go rs1 (p1 + l1)
            where
                go [] _ = []
                go ((ColouredRange p l c):rs) delta = [(p - delta, c), (l, DefaultColourGroup)] ++ go rs (delta + l + p)
                removeZeroLengths ((l, _):(0, c):rs) = removeZeroLengths ((l, c):rs)
                removeZeroLengths (r:rs) = r : removeZeroLengths rs
                removeZeroLengths [] = []

getBoundingRect :: EditorDrawingOptions -> [DrawableLine] -> (PointD, PointD)
getBoundingRect opts dLines = (Point textLeft textTop, Point textRight textBottom)
    where
        fAscent = fontAscent opts
        lHeight = lineHeight opts
        textLeft = 7
        textTop = 5 + fAscent
        textRight = 7 + textLeft + (if null dLines then 0 else maximum . map sum . drawableLineWidths $ dLines)
        textBottom = textTop + (if null dLines then 0 else lHeight + (lineTop . last $ dLines))
        lineTop (DrawableLine t _ _) = t

drawableLineWidths :: [DrawableLine] -> [[Double]]
drawableLineWidths = map (\(DrawableLine _ w _) -> w)

newDrawableLineData :: PangoContext -> SourceData -> EditorDrawingOptions -> IO [DrawableLine]
newDrawableLineData pangoContext source opts = do
    let linesToDraw = textLines source
    (pangoLineShapes, lineWidths) <- getLineShapesWithWidths pangoContext (map (\l->(l, [ColouredRange 3 5 (ColourGroup 1), ColouredRange 9 3 (ColourGroup 1)]))linesToDraw)
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
            let drawBgPath [] = return()
                drawBgPath ((Point px py):ps) = do
                    moveTo px py
                    mapM_ (\(Point lx ly) -> lineTo lx ly) ps
                    closePath
                    setSourceRGB 0.9 0 0
                    strokePreserve
                    setSourceRGBA 0.69 0.65 0.5 0.3
                    fill
            _ <- traverse (traverse drawBgPath) $ backgroundPaths dData
            drawText (drawableLines dData)
            drawCursor dData opts
            restore
            where
                (Point textLeft textTop, Point textRight textBottom) = boundingRect dData

        drawText dLines = do
            setSourceRGBA 0 0.1 0 1
            mapM_ (\(DrawableLine yPos _ pangoShapes) -> do
                    moveTo 0 yPos
                    mapM_ drawTextPiece pangoShapes
                ) dLines
            where
                drawTextPiece (w, c, s) = do
                    setTextColour c
                    showGlyphString s
                    relMoveTo w 0
                setTextColour DefaultColourGroup = setSourceRGB 0 0 0
                setTextColour (ColourGroup _) = setSourceRGB 0.5 0.8 0.4

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

getEditorFromNotebookTab :: Widget -> IO (Maybe DrawingArea)
getEditorFromNotebookTab currentlyActiveEditor =
    if currentlyActiveEditor `isA` gTypeScrolledWindow then do
        let textEditScroller = castToScrolledWindow currentlyActiveEditor
        textEdit <- binGetChild textEditScroller
        let areaa = (\area -> if isA area gTypeDrawingArea then Just area else Nothing) =<< textEdit
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
