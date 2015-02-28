module Hob.Ui.Editor.Fancy (
    newEditorForText
    ) where

import           Control.Concurrent.MVar             (MVar, modifyMVar_,
                                                      newMVar, readMVar)
import           Control.Monad.Reader
import qualified Control.Monad.State.Lazy            as S
import           Data.Char                           (isPrint, isSpace)
import qualified Data.Foldable                       as F
import qualified Data.Function                       as F
import           Data.Generics
import           Data.Graph
import           Data.List                           (groupBy, sortBy, nubBy, sort)
import           Data.Maybe                          (catMaybes, listToMaybe,
                                                      maybeToList)
import  Control.Applicative
import qualified Data.Map as M
import Data.Map (Map)
import           Data.Prizm.Color
import           Data.Prizm.Color.CIE.LCH
import           Data.Text                           (Text, unpack)
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
import           Hob.Context.UiContext
import qualified IPPrint
import qualified Language.Haskell.HsColour           as HsColour
import qualified Language.Haskell.HsColour.Colourise as HsColour
import qualified Language.Haskell.HsColour.Output    as HsColour


data Block a = Block (Point a) (Point a) deriving Show
type Blocks a = Forest (Block a)


data VariableUsage = VariableUsage
                                [P.Name P.SrcSpanInfo] -- ^ defined var, list for destructuring matches | target <- usages
                                [P.Name P.SrcSpanInfo] -- ^ "depending on" variable usages
                                deriving Show

data VariableDependency = VariableDependency
                                (P.Name P.SrcSpanInfo) -- ^ definition of the "depending on" variable | source -> usages
                                [VariableUsage]        -- ^ usages of the definition
                                deriving Show

type ScopedVariableDependencies = Map (P.Match P.SrcSpanInfo) [VariableDependency]
type FunctionDef = (P.Match P.SrcSpanInfo, P.Name P.SrcSpanInfo, [P.Pat P.SrcSpanInfo], P.Rhs P.SrcSpanInfo, [P.Decl P.SrcSpanInfo])
type FunctionCache = Map (P.Decl P.SrcSpanInfo) [FunctionDef] -- this is still slow to lookup - find a better key for cache, maybe just an int tag for decls (e.g. generation int + srcspan)

data ColourGroup = DefaultColourGroup | ColourGroup Int deriving (Show, Eq)

data ColouredRange = ColouredRange
                        Int -- ^ pos
                        Int -- ^ length
                        ColourGroup -- ^ colour group
                    deriving (Show, Eq)

data SourceChangeEvent = InsertChar (Point Int) Char -- ^ Point (col, line) before the inserted char, Char that was inserted
                       | InsertLine (Point Int) -- ^ Point (col, line) before the inserted new line separator
                       | DeleteRange (Point Int) (Point Int) -- ^ Point (col, line) pair of start and end position of the deleted range
        deriving (Show)
        
data SourceData = SourceData {
    isModified                :: Bool,
    textLines                 :: [String],
    ast                       :: Maybe (P.Module P.SrcSpanInfo, [P.Comment]),
    parseErrorMessage         :: Maybe (Point Int, String),
    functionDefCache          :: FunctionCache,
    sourceBlocks              :: Blocks Int,
    varDeps                   :: ScopedVariableDependencies,
    varDepGraph               :: Graph,
    varDepGraphLookupByVertex :: Vertex -> (P.Name P.SrcSpanInfo, Int, [Int]),
    varDepGraphLookupByKey    :: Int -> Maybe Vertex
}

-- | Point x y
data Point a = Point a a deriving Show
type PointD = Point Double
type PointI = Point Int

--data DrawableG
-- | DrawableLine (y position) (x position of each char) [(width of the glyph, colour group, glyph to draw)]
data DrawableLine = DrawableLine Double [Double] [(Double, ColourGroup, GlyphItem)] 

type TextShapeCache = Map String [(GlyphItem, (PangoRectangle, PangoRectangle), [Double])]

data EditorDrawingData = EditorDrawingData {
    drawableLines      :: [DrawableLine],
    textToShapeCache   :: TextShapeCache,
    boundingRect       :: (PointD, PointD),
    cursorPosition     :: PointD,
    errorPosition      :: Maybe PointD,
    backgroundPaths    :: Forest [PointD],
    colourGroupToRgb   :: V.Vector (Double, Double, Double),
    sourceErrorMessage :: Maybe String
}

data EditorDrawingOptions = EditorDrawingOptions {
    lineHeight  :: Double,
    fontAscent  :: Double,
    fontDescent :: Double,
    reportError        :: Maybe String -> IO ()
}

data FancyEditor = FancyEditor {
    sourceData     :: SourceData,
    cursorPos      :: (Int, Int, Int), -- ^ X, Y, X_{navigation}
    drawingOptions :: EditorDrawingOptions,
    drawingData    :: EditorDrawingData
}


debugColourPrefs :: HsColour.ColourPrefs
debugColourPrefs = HsColour.defaultColourPrefs { HsColour.conid = [HsColour.Foreground HsColour.Yellow, HsColour.Bold], HsColour.conop = [HsColour.Foreground HsColour.Yellow], HsColour.string = [HsColour.Foreground HsColour.Green], HsColour.char = [HsColour.Foreground HsColour.Cyan], HsColour.number = [HsColour.Foreground HsColour.Red, HsColour.Bold], HsColour.layout = [HsColour.Foreground HsColour.White], HsColour.keyglyph = [HsColour.Foreground HsColour.White] }

debugPrint :: Show a => a -> IO()
debugPrint = putStrLn . HsColour.hscolour (HsColour.TTYg HsColour.XTerm256Compatible) debugColourPrefs False False "" False . IPPrint.pshow

tracePrint :: Show a => a -> a
tracePrint v = trace (HsColour.hscolour (HsColour.TTYg HsColour.XTerm256Compatible) debugColourPrefs False False "" False . IPPrint.pshow $ v) v

tracePrintOther :: Show a => a -> b -> b
tracePrintOther p v = trace (HsColour.hscolour (HsColour.TTYg HsColour.XTerm256Compatible) debugColourPrefs False False "" False . IPPrint.pshow $ p) v


findVars :: P.Exp P.SrcSpanInfo -> [P.Name P.SrcSpanInfo]
findVars (P.Var _ (P.UnQual _ name)) = [name]
findVars _ = []

findPatternVars :: P.Pat P.SrcSpanInfo -> [P.Name P.SrcSpanInfo]
findPatternVars (P.PVar _ name) = [name]
findPatternVars _ = []

findNames :: P.Name P.SrcSpanInfo -> [P.Name P.SrcSpanInfo]
findNames name = [name]

findNamesInPatterns :: [P.Pat P.SrcSpanInfo] -> [P.Name P.SrcSpanInfo]
findNamesInPatterns = concatMap (findElements findPatternVars)

findGenerators :: P.Stmt P.SrcSpanInfo -> [(P.Pat P.SrcSpanInfo, P.Exp P.SrcSpanInfo)]
findGenerators (P.Generator _ pat expr) = [(pat, expr)]
findGenerators _ = []

findQualifiers :: P.Stmt P.SrcSpanInfo -> [P.Exp P.SrcSpanInfo]
findQualifiers (P.Qualifier _ expr) = [expr]
findQualifiers _ = []

bindsToDecls :: Maybe (P.Binds l) -> [P.Decl l]
bindsToDecls (Just (P.BDecls _ decls)) = decls
bindsToDecls _ = []

findFuncs :: P.Decl P.SrcSpanInfo -> [FunctionDef]
findFuncs (P.FunBind _ matches) = concatMap funcMatches matches
    where
        funcMatches :: P.Match P.SrcSpanInfo -> [FunctionDef]
        funcMatches match@(P.Match _ name pats rhs binds) = [(match, name, pats, rhs, bindsToDecls binds)]
        funcMatches match@(P.InfixMatch _ pat name pats rhs binds) = [(match, name, pat:pats, rhs, bindsToDecls binds)]

findFuncs _ = []


findPatternBinds :: P.Decl P.SrcSpanInfo -> [(P.Pat P.SrcSpanInfo, P.Rhs P.SrcSpanInfo, [P.Decl P.SrcSpanInfo])]
findPatternBinds (P.PatBind _ pat rhs binds) = [(pat, rhs, bindsToDecls binds)]
findPatternBinds _ = []

findElements :: (Data a, Typeable b) => (b -> [c]) -> a -> [c]
findElements query a = ([] `mkQ` query $ a) ++ (concat $ gmapQ (findElements query) a)

findElementsNonRec :: (Data a, Typeable b) => (b -> [c]) -> a -> [c]
findElementsNonRec query a = let res = [] `mkQ` query $ a
                             in if null res then (concat $ gmapQ (findElementsNonRec query) a)
                             else res

findVariableDependencies :: FunctionCache -> Maybe ScopedVariableDependencies -> [P.Decl P.SrcSpanInfo] -> (FunctionCache, ScopedVariableDependencies)
findVariableDependencies funcCache oldVarDeps decls = 
    let (funcs, funcCache') = S.runState findFuncs' funcCache 
        deps = varDepsForFunctions funcs
    in (funcCache', deps)
    where
        varDepsForFunctions = foldr (\f m -> M.insert (varDepMapKey f) (varDepsForFunctionMatch f) m) M.empty

        varDepsForFunctionMatch f = maybe (findVarDeps f) id $ M.lookup (varDepMapKey f) =<< oldVarDeps
        
        varDepMapKey (match, _, _, _, _) = match
        
        insertVarDep varDep [] = [varDep]
        insertVarDep (varDep@(VariableDependency vName vUsage)) ((dep@(VariableDependency dName dUsage)):deps)
         | vName == dName = (VariableDependency dName (vUsage++dUsage)):deps
         | otherwise = dep:insertVarDep varDep deps

        findFuncs' :: S.State FunctionCache [FunctionDef]
        findFuncs' = (return . concat) =<< mapM _findFuncs decls
            where
                _findFuncs :: P.Decl P.SrcSpanInfo -> S.State FunctionCache [FunctionDef]
                _findFuncs d = do
                    c <- S.get
                    case M.lookup d c of
                        Nothing -> do
                                    let funcs = findElements findFuncs d
                                    S.put $ M.insert d funcs c
                                    return funcs
                        Just funcs -> return funcs
            

        findVarDeps = foldr insertVarDep [] . patternDependencies -- TODO subDelcs, do notation
            where
                -- | finds patterns used in rhs
                patternDependencies (_, fncName, patterns, rhs, subDecls) = patternUsage
                    where
                        filterNames :: P.Name P.SrcSpanInfo -> [P.Name P.SrcSpanInfo] -> [P.Name P.SrcSpanInfo]
                        filterNames name = filter (name P.=~=)

                        patternNames :: [P.Name P.SrcSpanInfo]
                        patternNames = findNamesInPatterns patterns

                        subPatternNames :: [P.Name P.SrcSpanInfo]
                        subPatternNames = concatMap (\(pat, _, _) -> findElements findPatternVars pat) $ concatMap ([] `mkQ` findPatternBinds) subDecls

                        subFuncNames :: [P.Name P.SrcSpanInfo]
                        subFuncNames = concatMap (\(_, name, _, _, _) -> [name]) $ concatMap ([] `mkQ` findFuncs) subDecls

                        generators :: [([P.Name P.SrcSpanInfo], [P.Name P.SrcSpanInfo])]
                        generators = map (\(pat, expr) -> (findElements findNames pat, findElements findVars expr)) $ (findElements findGenerators rhs ++ findElements findGenerators subDecls)

                        qualifiers :: [([P.Name P.SrcSpanInfo], [P.Name P.SrcSpanInfo])]
                        qualifiers = map (\expr -> ([], findElements findVars expr)) $ (findElements findQualifiers rhs ++ findElements findQualifiers subDecls)

                        subPatterns :: [([P.Name P.SrcSpanInfo], [P.Name P.SrcSpanInfo])]
                        subPatterns = map (\(pat, patternRhs, patternDecls) -> (findElements findNames pat, (findElements findNames patternRhs) ++ concatMap (findElements findNames) patternDecls)) $ findElements findPatternBinds subDecls

                        variables :: [([P.Name P.SrcSpanInfo], [P.Name P.SrcSpanInfo])]
                        variables = [([fncName], findElements findVars rhs)]

                        patternUsage :: [VariableDependency]
                        patternUsage = concatMap (\name->catMaybes $ map (\(pvars, evars)->let usage = filterNames name evars in if null usage then Nothing else Just $ VariableDependency name [VariableUsage pvars usage]) (variables++qualifiers++generators++subPatterns)) $ nubBy (P.=~=) (patternNames++subFuncNames++subPatternNames)

newSourceDataFromText :: Text -> IO SourceData
newSourceDataFromText text = newSourceData Nothing newTextLines
    where
        newTextLines = lines . unpack $ text

-- TODO: vector for lines?
newSourceData :: Maybe (SourceData, [SourceChangeEvent]) -> [String] -> IO SourceData
newSourceData sourceHistory newTextLines = do
--    debugPrint $ parseErrorMessage sd
    return sd
    where
        oldSourceData = fmap fst sourceHistory
        sourceChangeEvents = fmap snd sourceHistory
        collectSourceInfoSpans = sort . concatMap (F.foldr (\a s -> P.srcInfoSpan a : s) [])
        srcLocToPoint (P.SrcLoc _ l c) = Point (c-1) (l-1)
        nestInfoSpans :: [P.SrcSpan] -> Forest P.SrcSpan
        nestInfoSpans [] = []
        nestInfoSpans (el:els) = Node el (nestInfoSpans inTheRangeEls) : (nestInfoSpans furtherEls)
            where
                (inTheRangeEls, furtherEls) = break (isAfter el) . dropWhile (el == ) $ els
                isAfter a b = (P.srcSpanEndLine b > P.srcSpanEndLine a) || ((P.srcSpanEndLine b == P.srcSpanEndLine a) && (P.srcSpanEndLine b > P.srcSpanEndLine a))
        infoSpanToBlock s = Block
                                (Point (P.srcSpanStartColumn s - 1) (P.srcSpanStartLine s - 1))
                                (Point (P.srcSpanEndColumn s - 1) (P.srcSpanEndLine s - 1))
        collectSourceBlocks = (fmap (fmap infoSpanToBlock)) . nestInfoSpans . collectSourceInfoSpans
        (parsedModule, errorMessage) = case ast =<< oldSourceData of
                        Nothing -> parseFullModule
                        Just oldAst -> let notChangedDecls = (filter isNotChangedDecl . allCachedDecls $ oldAst)
                                          in addStaleDeclsOnFailure $ addCachedDecls (parseModuleMinusDecls notChangedDecls) notChangedDecls
            where
                parseModuleLines textToParse = case P.parseFileContentsWithComments P.defaultParseMode . unlines $ textToParse of
                    P.ParseOk astInfo -> (Just astInfo, Nothing)
                    P.ParseFailed srcLoc msg -> (Nothing, Just (srcLoc, msg))                                
                parseFullModule = parseModuleLines newTextLines

                allCachedDecls (P.Module _ _ _ _ decls, _) = decls
                allCachedDecls _ = []

                isNotChangedDecl decl = maybe False (null . filter eventChangesDecl) sourceChangeEvents
                    where
                        declInfoSpan = P.srcInfoSpan . P.ann $ decl
                        startLine = P.srcSpanStartLine declInfoSpan
                        endLine = P.srcSpanEndLine declInfoSpan
                        startCol = P.srcSpanStartColumn declInfoSpan
                        endCol = P.srcSpanEndColumn declInfoSpan
                        declContainsPoint pt = ((line == startLine) && (col >= startCol) && (line < endLine))
                                                     || ((line == endLine) && (col <= endCol) && (line > startLine))
                                                     || ((line == startLine) && (col >= startCol) && (line == endLine) && (col <= endCol))
                                                     || ((line > startLine) && (line < endLine))
                            where
                                col = _c + 1
                                line = _l + 1
                                (Point _c _l) = pt
                        declIsInBetweenPoints p1 p2 = ((line1 == startLine) && (col1 <= startCol) && (line2 > endLine))
                                                     || ((line2 == endLine) && (col2 >= endCol) && (line1 < startLine))
                                                     || ((line1 == startLine) && (col1 <= startCol) && (line2 == endLine) && (col2 >= endCol))
                                                     || ((line1 < startLine) && (line2 > endLine))
                            where
                                col1 = _c1 + 1
                                line1 = _l1 + 1
                                col2 = _c2 + 1
                                line2 = _l2 + 1
                                (Point _c1 _l1) = p1
                                (Point _c2 _l2) = p2
                        eventChangesDecl (InsertChar p _) = declContainsPoint p
                        eventChangesDecl (InsertLine p) = declContainsPoint p
                        eventChangesDecl (DeleteRange p1 p2) = declContainsPoint p1 || declContainsPoint p2 || declIsInBetweenPoints p1 p2

                parseModuleMinusDecls decls = parseModuleLines linesToParse
                    where
                        linesToParse = foldr blankOutRange newTextLines rangesToBlankout
                        blankOutRange (P.SrcSpan _ startLine startCol endLine endCol) tLines = prefix ++ body ++ suffix
                        -- TODO: optimize this!
                            where
                                prefix = take (startLine - 1) tLines
                                body = if (endLine == startLine) then singleLineBody else multiLineBody
                                singleLineBody =  (map (\l -> (take (startCol - 1) l) ++ (replicate (endCol - startCol +1) ' ') ++ (drop endCol l)) . take 1 . drop (startLine - 1) $ tLines)
                                multiLineBody =  (map (take (startCol - 1)) . take 1 . drop (startLine - 1) $ tLines)
                                              ++ (replicate (endLine - startLine - 1) "")
                                              ++ (map (\l -> (replicate (endCol) ' ') ++ (drop endCol l)) . take 1 . drop (endLine - 1) $ tLines)
                                suffix = (drop (endLine ) $ tLines)
                        rangesToBlankout = map (P.srcInfoSpan . adjustInfoSpanByEvents . P.ann) decls
                addCachedDecls (Just (P.Module loc headers pragmas imports decls, comments), err) oldDecls = (Just (P.Module loc headers pragmas imports ((updateDeclsByEvents oldDecls)++decls), comments), err)
                addCachedDecls (_, err) _ = (Nothing, err)

                addStaleDeclsOnFailure (Nothing, err) = (fmap updateRangesByEvents $ ast =<< oldSourceData, err)
                    where
                        updateRangesByEvents (P.Module loc headers pragmas imports decls, comments) = (P.Module loc headers pragmas imports (updateDeclsByEvents decls), comments)
                        updateRangesByEvents m = m
                addStaleDeclsOnFailure (Just m, err) = (Just m, err)
                
                updateDeclsByEvents = fmap updateDecl
                    where
                        updateDecl decl = fmap adjustInfoSpanByEvents decl

                adjustInfoSpanByEvents range = maybe range (foldr adjustInfoSpanByEvent range) sourceChangeEvents

                adjustInfoSpanByEvent (InsertChar (Point pColumn pLine) _) range@(P.SrcSpanInfo (P.SrcSpan file startLine startCol endLine endCol) _) = 
                        if needChange then P.noInfoSpan $ P.SrcSpan file startLine startCol' endLine endCol'
                        else range
                    where
                        needChange = (startLine == (pLine+1) && startCol >= (pColumn+1)) || (endLine == (pLine+1) && endCol >= (pColumn+1))
                        startCol' = if (startLine == (pLine+1) && startCol >= (pColumn+1)) then startCol + 1 else startCol
                        endCol' = if (endLine == (pLine+1) && endLine >= (pColumn+1)) then endCol + 1 else endCol
                adjustInfoSpanByEvent (InsertLine (Point pColumn pLine)) range@(P.SrcSpanInfo (P.SrcSpan file startLine startCol endLine endCol) _) = 
                        if needChange then P.noInfoSpan $ P.SrcSpan file startLine' startCol' endLine' endCol'
                        else range
                    where
                        needChange = (startLine == (pLine+1) && startCol >= (pColumn+1)) || (endLine == (pLine+1) && endCol >= (pColumn+1)) || (endLine > (pLine+1)) 
                        startCol' = if (startLine == (pLine+1) && startCol >= (pColumn+1)) then startCol - (pColumn+1) else startCol
                        endCol' = if (endLine == (pLine+1) && endLine >= (pColumn+1)) then endCol - (pColumn+1) else endCol
                        startLine' = if (startLine == (pLine+1) && startCol >= (pColumn+1)) || (startLine > (pLine+1)) then startLine + 1 else startLine
                        endLine' = if (endLine == (pLine+1) && endCol >= (pColumn+1)) || (endLine > (pLine+1)) then endLine + 1 else endLine
                adjustInfoSpanByEvent (DeleteRange (Point sColumn sLine) (Point eColumn eLine)) range@(P.SrcSpanInfo (P.SrcSpan file startLine startCol endLine endCol) _) = 
                        if needChange then P.noInfoSpan $ P.SrcSpan file startLine' startCol' endLine' endCol'
                        else range
                    where -- TODO: merge logic with the InsertChar handler, adjust start and end points independently rather than using maxes
                        needChange = (startLine == (sLine+1) && startCol >= (sColumn+1)) || (endLine == (eLine+1) && endCol >= (eColumn+1)) || (endLine > (eLine+1))
                        lineAdj = eLine - sLine
                        colAdj = if sLine == eLine then eColumn - sColumn else eColumn
                        startLine' = max (sLine + 1) (startLine - lineAdj)
                        startCol' = if (startLine == (eLine+1) && startCol >= (eColumn+1)) then max (sColumn+1) (startCol - colAdj) else startCol
                        endLine' = max startLine' (endLine - lineAdj)
                        endCol' = if (endLine == (eLine+1) && endLine >= (eColumn+1)) then if (sLine==eLine) then max startCol' (endCol - colAdj) else endCol - colAdj else endCol
                
        declarations (Just (P.Module _ _ _ _ decl, _)) = decl
        declarations _ = []
        (funcDefCache, newVarDeps) = findVariableDependencies (maybe M.empty functionDefCache oldSourceData) (fmap varDeps oldSourceData) . declarations $ parsedModule
        viariablesWithIds = zip [0..] . map (\(VariableDependency v _) -> v) . concat . M.elems $ newVarDeps
        foundDepsForVariable v = concat . concat . maybeToList $ foundVarDeps (concat . M.elems $ newVarDeps) >>= Just . map (\(VariableUsage targets _) -> targets)
            where
                foundVarDeps ((VariableDependency var usages):vars) = if var == v then Just usages else foundVarDeps vars
                foundVarDeps [] = Nothing
        varToId v = foundVarId viariablesWithIds
            where
                foundVarId ((idx, var):vars) = if var == v then Just idx else foundVarId vars
                foundVarId [] = Nothing
        varDepGraphNodes = map (\(idx, var) -> (var, idx, catMaybes . (map varToId) . foundDepsForVariable $ var)) viariablesWithIds
        (newVarDepGraph, newVarDepGraphVertexToNode, newVarDepGraphKeyToVertex) = graphFromEdges varDepGraphNodes
        sd = SourceData {
            isModified = False,
            textLines = newTextLines,
            ast = parsedModule,
            parseErrorMessage = fmap (\(loc, msg) -> (srcLocToPoint loc, msg)) errorMessage,
            functionDefCache = funcDefCache,
            sourceBlocks = collectSourceBlocks $ declarations parsedModule,
            varDeps = newVarDeps,
            varDepGraph = transposeG newVarDepGraph,
            varDepGraphLookupByVertex = newVarDepGraphVertexToNode,
            varDepGraphLookupByKey = newVarDepGraphKeyToVertex
        }

newDrawingData :: Maybe EditorDrawingData -> PangoContext -> SourceData -> (Int, Int, Int) -> EditorDrawingOptions -> IO EditorDrawingData
newDrawingData oldData pangoContext source (cursorCharNr, cursorLineNr, _) opts = do
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
                stepToHue step = (fromIntegral step) * (fromIntegral amount) / goldenRatio
                generateColor l c h = CIELCH ((l/4.0+0.2)*100.0) ((0.6+c/2.0)*100.0) (h*360.0)
        cielChToRgbTuple c = let (RGB r g b) = toRGB c
                             in ((fromIntegral r)/255.0, (fromIntegral g)/255.0, (fromIntegral b)/255.0)
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
        variableDepsGraphEdges = V.fromList . map (cielChToRgbTuple . vertexDepColour) $ variableVertexesInTopoOrder
            where
                uniqueVarColours = V.fromList . generateCielCHColours $ length . concat . M.elems . varDeps $ source
                vertexDepColour v = let (_, idx, deps) = varDepGraphLookupByVertex source v
                                    in combineColours (idx:deps)
                    where
                        combineColours ids = foldl1 addColourInFold (idsToColours ids)
                            where
                                idsToColours = map (uniqueVarColours V.!)
                                percent = toInteger $ 100 `quot` (length ids)
                                addColourInFold c1 c2 = interpolate percent (c1, c2)
                variableVertexesInTopoOrder = reverse . topSort $ varDepGraph source
        formattedErrorMessage = fmap formatError . parseErrorMessage $ source
            where
                formatError ((Point c l), msg) = msg ++ " at "++(show l)++":"++(show c)
        ed textShapesCache lineData cursorP errorPos = EditorDrawingData {
            drawableLines = lineData,
            textToShapeCache = textShapesCache,
            boundingRect = getBoundingRect opts lineData,
            cursorPosition = cursorP,
            errorPosition = errorPos,
            backgroundPaths = convertBlocks lineData (sourceBlocks source),
            colourGroupToRgb = variableDepsGraphEdges,
            sourceErrorMessage = formattedErrorMessage
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

-- TODO: maybe add pango context, widget, scrolling widget
newFancyEditor :: Label -> PangoContext -> Text -> IO FancyEditor
newFancyEditor errLabel pangoContext text = do
    dOptions <- newDrawingOptions pangoContext errorReporter
    sd <- newSourceDataFromText text
    dd <- newDrawingData Nothing pangoContext sd initialCursor dOptions
    return FancyEditor {
        sourceData = sd,
        cursorPos = initialCursor,
        drawingOptions = dOptions,
        drawingData = dd
    }
    where
        initialCursor = (0, 0, 0)
        errorReporter (Just err) = do
            labelSetText errLabel err
            widgetShowAll errLabel
        errorReporter Nothing = widgetHide errLabel

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
            return scrolledWindow

        newPangoContext = do
            pangoContext <- cairoCreateContext Nothing
            fontDescription <- fontDescriptionNew
            fontDescriptionSetFamily fontDescription "Ubuntu Mono"
            fontDescriptionSetSize fontDescription 12
            contextSetFontDescription pangoContext fontDescription
            return pangoContext

        createNewEditor ctx = do
            editorWidget <- newEditorWidget =<< idGenerator ctx
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

            fancyEditorDataHolder <- newMVar =<< newFancyEditor errorLabel pangoContext text

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
    dData <- S.gets drawingData
    dData' <- liftIO $ newDrawingData (Just dData) pangoContext source cursor opts
    S.modify $ \ed -> ed{drawingData = dData'}

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

moveCursorToTheEndOfTheLine :: S.StateT FancyEditor IO ()
moveCursorToTheEndOfTheLine = do
    source <- S.gets sourceData
    (cx, cy, _) <- S.gets cursorPos
    let tLines = textLines source
        (preLines, postLines) = splitAt cy tLines
        cx' = case take 1 $ postLines of
            [l] -> length l
            _ -> 0
    S.modify $ \ed -> ed{cursorPos = (cx', cy, cx')}

moveCursorToTheStartOfTheLine :: S.StateT FancyEditor IO ()
moveCursorToTheStartOfTheLine = do
    (_, cy, _) <- S.gets cursorPos
    S.modify $ \ed -> ed{cursorPos = (0, cy, 0)}

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
    source' <- liftIO $ newSourceData (Just (source, [InsertChar (Point cx cy) c])) tLines'
    let source'' = source'{isModified = True}
    S.modify $ \ed -> ed{sourceData = source''}
    updateCursorX 1

deleteCharacterRange :: Point Int -> Point Int -> S.StateT FancyEditor IO ()
deleteCharacterRange startPoint@(Point sx sy) endPoint@(Point ex ey) = do
    source <- S.gets sourceData
    let tLines = textLines source
        (preLines, postLines1) = splitAt sy tLines
        postLines2 = drop (ey-sy) postLines1
        startLine1 = map (take sx) . take 1 $ postLines1
        startLine2 = map (drop ex) . take 1 $ postLines2
        startLine = (++) <$> startLine1 <*> startLine2
        tLines' = preLines ++ startLine ++ (drop 1 postLines2)
    source' <- liftIO $ newSourceData (Just (source, [DeleteRange startPoint endPoint])) tLines'
    let source'' = source'{isModified = True}
    S.modify $ \ed -> ed{sourceData = source'', cursorPos = (sx, sy, sx)}
        

deleteCharactersFromCursor :: Int -> S.StateT FancyEditor IO ()
deleteCharactersFromCursor amount = do
    source <- S.gets sourceData
    (cx, cy, _) <- S.gets cursorPos
    let tLines = textLines source
        (preLines, postLines) = splitAt cy tLines
        (startx, endx) = if amount > 0 then (cx, cx+amount) else (cx+amount, cx)
        (startx', adjsy) = adjustNegativePosByLengths (cx : (map length . reverse $ preLines)) startx
        starty' = max 0 $ cy + adjsy
        (endx', adjey) = adjustPositivePosByLengths (map length $ postLines) endx
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
    (cx, cy, _) <- S.gets cursorPos
    let tLines = textLines source
        (preLines, postLines) = splitAt cy tLines
        splitLines = concat . fmap (\l -> let (preChars, postChars) = splitAt cx l
                                  in [preChars, postChars]
                            ) . take 1 $ postLines
        tLines' = preLines ++ splitLines ++ drop 1 postLines
    source' <- liftIO $ newSourceData (Just (source, [InsertLine (Point cx cy)])) tLines'
    let source'' = source'{isModified = True}
    S.modify $ \ed -> ed{sourceData = source'', cursorPos=(0, cy+1, 0)}

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
            
    let getLinesInOnePage = liftIO $ do
            widgetHeight <- widgetGetAllocatedHeight scrolledWindow
            editor <- readMVar fancyEditorDataHolder
            return $ floor $ (fromIntegral widgetHeight) / (lineHeight . drawingOptions $ editor)

    case (modifiers, unpack $ keyName keyValue) of
        ([], "Right") -> invokeEditorCmd $ updateCursorX 1
        ([], "Left") -> invokeEditorCmd $ updateCursorX (-1)
        ([], "Up") -> invokeEditorCmd $ updateCursorY (-1)
        ([], "Down") -> invokeEditorCmd $ updateCursorY 1
        ([], "Page_Up") -> do
            linesToJump <- getLinesInOnePage
            invokeEditorCmd $ updateCursorY (-linesToJump)
        ([], "Page_Down") -> do
            linesToJump <- getLinesInOnePage
            invokeEditorCmd $ updateCursorY linesToJump
        ([], "Home") -> invokeEditorCmd $ moveCursorToTheStartOfTheLine
        ([], "End") -> invokeEditorCmd $ moveCursorToTheEndOfTheLine
        ([], "BackSpace") -> invokeEditorCmd $ deleteCharactersFromCursor (-1)
        ([], "Delete") -> invokeEditorCmd $ deleteCharactersFromCursor 1
        ([], "Return") -> invokeEditorCmd $ insertNewLine
        _ -> maybe ((liftIO $ debugPrint (modifiers, unpack $ keyName keyValue)) >> return False) (\c -> invokeEditorCmd $ insertEditorChar c) printableChar

-- | [(String, [(Int, Int)]) = [(line, [ColouredRange])]
getLineShapesWithWidths :: TextShapeCache -> PangoContext -> [(String, [ColouredRange])] -> IO (TextShapeCache, [[(Double, ColourGroup, GlyphItem)]], [[Double]])
getLineShapesWithWidths textShapeCache pangoContext linesToDraw = do
    (pangoLineShapeData, textShapeCache') <- S.runStateT (mapM (\partLine -> do
            (return . concat) =<< mapM partLineToShapes partLine
        ) lineParts) textShapeCache

    let pangoLineShapes = map (map (\(s, _, _) -> s)) pangoLineShapeData
        extents = map (map (\(_, e, _) -> e)) pangoLineShapeData
        lineWordWidths = map (map (\(_, _, w) -> w)) pangoLineShapeData
    return (textShapeCache', sizedLineShapes pangoLineShapes extents, map concat lineWordWidths)
    where
        lineParts = map (\(line, pieces) -> listToPieces (relativePiecePositions pieces) line) breakPointedLines

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

        sizedLineShapes pangoLineShapes extents = zipWith3 (\le lc ls -> zipWith3 (\e c s -> (glyphWidthFromExtent e, c, s)) le lc ls) extents ((map (coloursOfThePieces.snd) breakPointedLines)) pangoLineShapes

        glyphWidthFromExtent (_, (PangoRectangle _ _ w _)) = w

        listToPieces :: [Int] -> [a] -> [[a]]
        listToPieces [] l = [l]
        listToPieces (p:ps) l = let (l1, lRest) = splitAt p l
                                in l1:listToPieces ps lRest
        breakPointedLines = map (\(line, ranges) -> (line, colouredRangesToBreakPoints ranges)) linesToDraw
        relativePiecePositions pieces = map fst pieces
        coloursOfThePieces pieces = map snd pieces
        colouredRangesToBreakPoints [] = [(0, DefaultColourGroup)]
        colouredRangesToBreakPoints ((ColouredRange p1 l1 c1):rs1) = removeZeroLengths $ [(0, DefaultColourGroup), (p1, c1), (l1, DefaultColourGroup)] ++ go rs1 (p1 + l1)
            where
                go [] _ = []
                go ((ColouredRange p l c):rs) delta = [(p - delta, c), (l, DefaultColourGroup)] ++ go rs (l + p)
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

varDependenciesToColourGroupLines :: [VariableDependency] -> [[ColouredRange]]
varDependenciesToColourGroupLines vars = linedItemsToListPositions $ makeUnique $ sortBy compareLinesWithColouredRanges $ concatMap convertDefinition $ zip vars [0..]
    where
        convertDefinition ((VariableDependency def usageDefs), colourGroup) = srcElementToLineAndColourRange def : concatMap (\(VariableUsage _ usages) -> map srcElementToLineAndColourRange (usages)) usageDefs
            where
                srcElementToLineAndColourRange srcElement = (startLine, ColouredRange startPos definitionLength (ColourGroup colourGroup))
                    where
                        srcSpan = P.srcInfoSpan . P.ann $ srcElement
                        startLine = P.srcSpanStartLine srcSpan - 1
                        startPos = P.srcSpanStartColumn srcSpan - 1
                        definitionLength = P.srcSpanEndColumn srcSpan - startPos - 1
        linedItemsToListPositions = convertLineNrToPosInList 0 . moveLineNrUp . (groupBy ((==) `F.on` fst))
            where
                moveLineNrUp = map (\items -> (fst . head $ items, map snd items))
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



newDrawableLineData :: TextShapeCache -> PangoContext -> SourceData -> EditorDrawingOptions -> IO (TextShapeCache, [DrawableLine])
newDrawableLineData textShapeCache pangoContext source opts = do
    let linesToDraw = textLines source
        colourGroups = varDependenciesToColourGroupLines . concat . M.elems . varDeps $ source
--    debugPrint $ colourGroups !! 143
    (textShapeCache', pangoLineShapes, lineWidths) <- getLineShapesWithWidths textShapeCache pangoContext (zip linesToDraw (colourGroups++repeat []))
    return $ (textShapeCache', map (\(a, b, c) -> DrawableLine a b c) $ zip3 [i*h | i <- [0..]] lineWidths pangoLineShapes)
    where
        h = lineHeight opts

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

        drawPaths paths = traverse (traverse drawPath) paths >> return()
            where
                drawPath [] = return()
                drawPath ((Point px py):ps) = do
                    moveTo px py
                    mapM_ (\(Point lx ly) -> lineTo lx ly) ps
                    closePath
                    setSourceRGBA 0.3 0.3 0.4 0.05
                    strokePreserve
                    setSourceRGBA 0.69 0.65 0.5 0.15
                    fill

        drawContents (dData, opts) = do
            save
            translate textLeft textTop
            rect <- getClipRectangle
            drawPaths $ bgPathsToDraw rect
            drawErrorPointer dData opts
            drawText (linesToDraw rect) (colourGroupToRgb dData)
            drawCursor dData opts
            restore
            liftIO $ (reportError opts) $ sourceErrorMessage dData

            where
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
