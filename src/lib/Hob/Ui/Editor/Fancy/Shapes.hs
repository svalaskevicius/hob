module Hob.Ui.Editor.Fancy.Shapes (
  newDrawingData,
  newDrawingOptions
    ) where

import           Control.Monad.Reader
import qualified Control.Monad.State.Lazy            as S
import Control.Arrow ((&&&), second)
import           Data.Char                           (isSpace)
import qualified Data.Function                       as F
import           Data.Graph
import           Data.List                           (groupBy, sortBy)
import           Data.Maybe                          (listToMaybe)
import qualified Data.Map as M
import           Data.Prizm.Color
import           Data.Prizm.Color.CIE.LCH
import           Data.Text                           (Text)
import    qualified       Data.Text                as T

import qualified Data.Vector                         as V
import           Graphics.UI.Gtk                     hiding (Point)
import qualified Language.Haskell.Exts.Annotated     as P

import Hob.Ui.Editor.Fancy.Types


newDrawingData :: Maybe EditorDrawingData -> PangoContext -> SourceData -> CursorHead -> CursorHead -> EditorDrawingOptions -> IO EditorDrawingData
newDrawingData oldData pangoContext source cursor@(CursorHead cursorCharNr cursorLineNr _) selection@(CursorHead selectionCharNr selectionLineNr _) opts = do
--    debugPrint (varDepGraph source)
  --  debugPrint $ reverse . topSort $ varDepGraph source
    (textShapesCache, lineData) <- newDrawableLineData (maybe M.empty textToShapeCache oldData) pangoContext source opts
    let cursorP = sourceCoordsToDrawing lineData (Point cursorCharNr cursorLineNr)
        errorPos = fmap (sourceCoordsToDrawing lineData . fst) (parseErrorMessage source)
    return $ ed textShapesCache lineData cursorP errorPos
    where
        generateCielCHColours amount = [generateColor 0.2 1 (stepToHue i) | i <- [0..amount-1]]
            where
                goldenRatio = 1.61803398875
                stepToHue step = fromIntegral step * fromIntegral amount / goldenRatio
                generateColor l c h = CIELCH ((l/4.0+0.2)*100.0) ((0.6+c/2.0)*100.0) (h*360.0)
        cielChToRgbTuple c = let (RGB r g b) = toRGB c
                             in (fromIntegral r/255.0, fromIntegral g/255.0, fromIntegral b/255.0)
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
                        dBottomRight = movePointToMaxRightOfTheRange lineData tl br . movePointToLineBottom $ sourceCoordsToDrawing lineData br
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
                        lineTextRange = take (l2-l1) . drop l1 $ textLines source
                        lineWhiteSpacePrefixes :: [Text]
                        lineWhiteSpacePrefixes = map (T.takeWhile isSpace) lineTextRange
                        lineWhiteSpacePrefixLengths :: [Int]
                        lineWhiteSpacePrefixLengths = map T.length lineWhiteSpacePrefixes
                        lineWhiteSpacePrefixWidths :: [Double]
                        lineWhiteSpacePrefixWidths = zipWith (\l w -> sum $ take l w) lineWhiteSpacePrefixLengths lineWidthRange
                        lineWidthRange :: [[Double]]
                        lineWidthRange = take (l2-l1) . drop l1 $ drawableLineWidths lineData
        movePointToLineBottom (Point px py) = Point px (py + lineHeight opts)
        movePointToMaxRightOfTheRange :: [DrawableLine] -> PointI -> PointI -> PointD -> PointD
        movePointToMaxRightOfTheRange lineData (Point _ l1) (Point _ l2) (Point px py) = Point (max px maxRight) py
            where
                maxRight = maximum . (0:) . map sum $ lineWidthRange
                    where
                        lineWidthRange = take (l2-l1) . drop l1 $ drawableLineWidths lineData
        movePointToMaxRightOfTheLine :: [DrawableLine] -> Int -> PointD -> PointD
        movePointToMaxRightOfTheLine lineData l (Point _ py) = Point maxRight py
            where
                maxRight = maximum . (0:) . map sum $ lineWidthRange
                    where
                        lineWidthRange = take 1 . drop l $ drawableLineWidths lineData
        variableDepsGraphEdges = V.fromList . map (cielChToRgbTuple . vertexDepColour) $ variableVertexesInTopoOrder
            where
                uniqueVarColours = V.fromList . generateCielCHColours $ length . concat . M.elems . varDeps $ source
                vertexDepColour v = let (_, idx, deps) = varDepGraphLookupByVertex source v
                                    in combineColours (idx:deps)
                    where
                        combineColours ids = foldl1 addColourInFold (idsToColours ids)
                            where
                                idsToColours = map (uniqueVarColours V.!)
                                percent = toInteger $ 100 `quot` length ids
                                addColourInFold c1 c2 = interpolate percent (c1, c2)
                variableVertexesInTopoOrder = reverse . topSort $ varDepGraph source
        formattedErrorMessage = fmap formatError . parseErrorMessage $ source
            where
                formatError (Point c l, msg) = msg ++ " at "++show l++":"++show c
        newSelectionContour lineData
          | cursor == selection = []
          | cursor < selection = textContour lineData (cursorCharNr, cursorLineNr) (selectionCharNr, selectionLineNr)
          | otherwise = textContour lineData (selectionCharNr, selectionLineNr) (cursorCharNr, cursorLineNr)
        textContour lineData (x1, y1) (x2, y2) = concatMap rightPoints [y1..y2] ++ (concatMap leftPoints . reverse $ [y1..y2])
            where
                rightPoints l = [topRightOfLine l, bottomRightOfLine l]
                leftPoints l = [bottomLeftOfLine l, topLeftOfLine l]
                topLeftOfLine l = if l > y1 then sourceCoordsToDrawing lineData (Point 0 l)
                                  else sourceCoordsToDrawing lineData (Point x1 y1)
                bottomLeftOfLine = movePointToLineBottom . topLeftOfLine
                topRightOfLine l = if l < y2 then movePointToMaxRightOfTheLine lineData l $ sourceCoordsToDrawing lineData (Point 0 l)
                                   else sourceCoordsToDrawing lineData (Point x2 y2)
                bottomRightOfLine = movePointToLineBottom . topRightOfLine

        ed textShapesCache lineData cursorP errorPos = EditorDrawingData {
            drawableLines = lineData,
            textToShapeCache = textShapesCache,
            boundingRect = getBoundingRect opts lineData,
            cursorPosition = cursorP,
            errorPosition = errorPos,
            backgroundPaths = convertBlocks lineData (sourceBlocks source),
            colourGroupToRgb = variableDepsGraphEdges,
            sourceErrorMessage = formattedErrorMessage,
            selectionContour = newSelectionContour lineData
        }

newDrawingOptions :: PangoContext -> (Maybe String -> IO()) -> IO EditorDrawingOptions
newDrawingOptions pangoContext errorReporter = do
    fontDescription <- contextGetFontDescription pangoContext
    fontSizeInfo <- contextGetMetrics pangoContext fontDescription emptyLanguage
    let a = ascent fontSizeInfo
        d = descent fontSizeInfo
    return EditorDrawingOptions {
        lineHeight = a + d + 5,
        fontAscent = a,
        fontDescent = d,
        reportError = errorReporter
    }

newDrawableLineData :: TextShapeCache -> PangoContext -> SourceData -> EditorDrawingOptions -> IO (TextShapeCache, [DrawableLine])
newDrawableLineData textShapeCache pangoContext source opts = do
    let linesToDraw = textLines source
        colourGroups = varDependenciesToColourGroupLines . concat . M.elems . varDeps $ source
--    debugPrint $ colourGroups !! 143
    (textShapeCache', pangoLineShapes, lineWidths) <- getLineShapesWithWidths textShapeCache pangoContext (zip linesToDraw (colourGroups++repeat []))
    return (textShapeCache', map (\(a, b, c) -> DrawableLine a b c) $ zip3 [i*h | i <- [0..]] lineWidths pangoLineShapes)
    where
        h = lineHeight opts

sourcePointToDrawingPoint :: [[Double]] -> PointI -> EditorDrawingOptions -> PointD
sourcePointToDrawingPoint lineWidths (Point cx cy) opts = Point cursorLeft cursorTop
    where
        cursorTop = fromIntegral cy * lHeight - fAscent
        maybeCursorLine = listToMaybe $ take 1 . drop cy $ lineWidths
        cursorLeft = maybe 0 (sum . take cx) maybeCursorLine
        lHeight = lineHeight opts
        fAscent = fontAscent opts

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

varDependenciesToColourGroupLines :: [VariableDependency] -> [[ColouredRange]]
varDependenciesToColourGroupLines vars = linedItemsToListPositions $ makeUnique $ sortBy compareLinesWithColouredRanges $ concatMap convertDefinition $ zip vars [0..]
    where
        convertDefinition (VariableDependency def usageDefs, colourGroup) = srcElementToLineAndColourRange def : concatMap (\(VariableUsage _ usages) -> map srcElementToLineAndColourRange usages) usageDefs
            where
                srcElementToLineAndColourRange srcElement = (startLine, ColouredRange startPos definitionLength (ColourGroup colourGroup))
                    where
                        srcSpan = P.srcInfoSpan . P.ann $ srcElement
                        startLine = P.srcSpanStartLine srcSpan - 1
                        startPos = P.srcSpanStartColumn srcSpan - 1
                        definitionLength = P.srcSpanEndColumn srcSpan - startPos - 1
        linedItemsToListPositions = convertLineNrToPosInList 0 . moveLineNrUp . groupBy ((==) `F.on` fst)
            where
                moveLineNrUp = map ((fst . head) &&& map snd)
                convertLineNrToPosInList _ [] = [[]]
                convertLineNrToPosInList line allItems@((l,item):items)
                 | line < l = replicate (l-line) [] ++ convertLineNrToPosInList l allItems
                 | line == l = item : convertLineNrToPosInList (l+1) items
                 | otherwise = error "unexpected order of colourRanges"
        compareLinesWithColouredRanges (l1, ColouredRange c1 _ _) (l2, ColouredRange c2 _ _)
            | lineComparison == EQ = columnComparison
            | otherwise = lineComparison
            where
                lineComparison = compare l1 l2
                columnComparison = compare c1 c2
        makeUnique (i1:i2:items)
           | i1 == i2 = makeUnique (i2:items)
           | otherwise = i1 : makeUnique (i2:items)
        makeUnique [i1] = [i1]
        makeUnique [] = []


-- | [(String, [(Int, Int)]) = [(line, [ColouredRange])]
getLineShapesWithWidths :: TextShapeCache -> PangoContext -> [(Text, [ColouredRange])] -> IO (TextShapeCache, [[(Double, ColourGroup, GlyphItem)]], [[Double]])
getLineShapesWithWidths textShapeCache pangoContext linesToDraw = do
    (pangoLineShapeData, textShapeCache') <- S.runStateT (mapM ((return . concat) <=< mapM partLineToShapes) lineParts) textShapeCache

    let pangoLineShapes = map (map (\(s, _, _) -> s)) pangoLineShapeData
        extents = map (map (\(_, e, _) -> e)) pangoLineShapeData
        lineWordWidths = map (map (\(_, _, w) -> w)) pangoLineShapeData
    return (textShapeCache', sizedLineShapes pangoLineShapes extents, map concat lineWordWidths)
    where
        lineParts = map (\(line, pieces) -> listToPieces (relativePiecePositions pieces) (T.unpack line)) breakPointedLines

        partLineToShapes partLine = do
            cache <- S.get
            case M.lookup partLine cache of
                Nothing -> do
                    shapes <- liftIO $ mapM (\part -> do
                                shape <- pangoShape part
                                extents <- glyphItemExtents shape
                                widths <- glyphItemGetLogicalWidths shape Nothing
                                return (shape, extents, widths)
                            ) =<< pangoItemize pangoContext partLine []
                    S.put (M.insert partLine shapes cache)
                    return shapes
                Just shapeData -> return shapeData

        sizedLineShapes pangoLineShapes extents = zipWith3 (zipWith3 (\e c s -> (glyphWidthFromExtent e, c, s))) extents (map (coloursOfThePieces.snd) breakPointedLines) pangoLineShapes

        glyphWidthFromExtent (_, PangoRectangle _ _ w _) = w

        listToPieces :: [Int] -> [a] -> [[a]]
        listToPieces [] l = [l]
        listToPieces (p:ps) l = let (l1, lRest) = splitAt p l
                                in l1:listToPieces ps lRest
        breakPointedLines = map (second colouredRangesToBreakPoints) linesToDraw
        relativePiecePositions = map fst
        coloursOfThePieces = map snd
        colouredRangesToBreakPoints [] = [(0, DefaultColourGroup)]
        colouredRangesToBreakPoints (ColouredRange p1 l1 c1:rs1) = removeZeroLengths $ [(0, DefaultColourGroup), (p1, c1), (l1, DefaultColourGroup)] ++ go rs1 (p1 + l1)
            where
                go [] _ = []
                go (ColouredRange p l c:rs) delta = [(p - delta, c), (l, DefaultColourGroup)] ++ go rs (l + p)
                removeZeroLengths ((l, _):(0, c):rs) = removeZeroLengths ((l, c):rs)
                removeZeroLengths (r:rs) = r : removeZeroLengths rs
                removeZeroLengths [] = []
