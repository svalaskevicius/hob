module Hob.Ui.Editor.Fancy.Parser (
  newSourceData,
  newSourceDataFromText,
  reverseEditorEvent,
  adjustPointByEvent
    ) where

import           Control.Applicative
import           Control.Arrow                   (first, (***))
import qualified Control.Monad.State.Lazy        as S
import qualified Data.Foldable                   as F
import           Data.Generics
import           Data.Graph
import           Data.List                       (nubBy, sort)
import qualified Data.Map                        as M
import           Data.Maybe                      (fromMaybe, mapMaybe,
                                                  maybeToList)
import           Data.Text                       (Text)
import qualified Data.Text                       as T

import qualified Language.Haskell.Exts.Annotated as P

import           Hob.Ui.Editor.Fancy.Types


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
findElements query a = ([] `mkQ` query $ a) ++ concat (gmapQ (findElements query) a)

-- findElementsNonRec :: (Data a, Typeable b) => (b -> [c]) -> a -> [c]
-- findElementsNonRec query a = let res = [] `mkQ` query $ a
--                              in if null res then concat $ gmapQ (findElementsNonRec query) a
--                              else res

findVariableDependencies :: FunctionCache -> Maybe ScopedVariableDependencies -> [P.Decl P.SrcSpanInfo] -> (FunctionCache, ScopedVariableDependencies)
findVariableDependencies funcCache oldVarDeps decls =
    let (funcs, funcCache') = S.runState findFuncs' funcCache
        deps = varDepsForFunctions funcs
    in (funcCache', deps)
    where
        varDepsForFunctions = foldr (\f m -> M.insert (varDepMapKey f) (varDepsForFunctionMatch f) m) M.empty

        varDepsForFunctionMatch f = fromMaybe (findVarDeps f) $ M.lookup (varDepMapKey f) =<< oldVarDeps

        varDepMapKey (match, _, _, _, _) = match

        insertVarDep varDep [] = [varDep]
        insertVarDep (varDep@(VariableDependency vName vUsage)) ((dep@(VariableDependency dName dUsage)):deps)
         | vName == dName = VariableDependency dName (vUsage++dUsage):deps
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
                        generators = map (findElements findNames *** findElements findVars) (findElements findGenerators rhs ++ findElements findGenerators subDecls)

                        qualifiers :: [([P.Name P.SrcSpanInfo], [P.Name P.SrcSpanInfo])]
                        qualifiers = map (\expr -> ([], findElements findVars expr)) (findElements findQualifiers rhs ++ findElements findQualifiers subDecls)

                        subPatterns :: [([P.Name P.SrcSpanInfo], [P.Name P.SrcSpanInfo])]
                        subPatterns = map (\(pat, patternRhs, patternDecls) -> (findElements findNames pat, findElements findNames patternRhs ++ concatMap (findElements findNames) patternDecls)) $ findElements findPatternBinds subDecls

                        variables :: [([P.Name P.SrcSpanInfo], [P.Name P.SrcSpanInfo])]
                        variables = [([fncName], findElements findVars rhs)]

                        patternUsage :: [VariableDependency]
                        patternUsage = concatMap (\name-> mapMaybe (\(pvars, evars)->let usage = filterNames name evars in if null usage then Nothing else Just $ VariableDependency name [VariableUsage pvars usage]) (variables++qualifiers++generators++subPatterns)) $ nubBy (P.=~=) (patternNames++subFuncNames++subPatternNames)

newSourceDataFromText :: Text -> IO SourceData
newSourceDataFromText text = newSourceData $ Right newTextLines
    where
        newTextLines = T.lines text

movePointByTextLines :: PointI -> [Text] -> PointI
movePointByTextLines (Point px py) tLines =
    if multiline then Point lastXOfLastLine py' else Point (px+lengthOfFirstLine) py
    where
        lineLength = length tLines
        multiline = lineLength > 1
        py' = py + lineLength - 1
        lastXOfLastLine = T.length . last $ tLines ++ [T.empty]
        lengthOfFirstLine = T.length . head $ tLines ++ [T.empty]


adjustPointByEvent :: SourceChangeEvent -> Point Int -> Point Int
adjustPointByEvent (InsertText (Point pColumn pLine) newLines) (Point px py) = Point px' py'
    where
        multiline = length newLines > 1
        px'
          | multiline = if pLine == py && pColumn <= px then T.length . last $ newLines else px
          | pLine == py && pColumn <= px = px + (T.length . head $ newLines)
          | otherwise = px
        py'
          | (pLine == py && pColumn <= px) || (pLine < py) = py + (length newLines - 1)
          | otherwise = py
adjustPointByEvent (DeleteText p@(Point sColumn sLine) tLines) (Point px py) = Point px' py'
    where
        lineAdj = eLine - sLine
        colAdj = if sLine == eLine then eColumn - sColumn else eColumn
        px' = if sLine == py && sColumn <= px then max sColumn (px - colAdj) else px
        py' = if py > sLine then max sLine (py - lineAdj) else py
        (Point eColumn eLine) = movePointByTextLines p tLines

reverseEditorEvent :: SourceChangeEvent -> [SourceChangeEvent]
reverseEditorEvent (InsertText p l) = [DeleteText p l]
reverseEditorEvent (DeleteText p l) = [InsertText p l]

-- TODO: vector for lines?
newSourceData :: Either (SourceData, [SourceChangeEvent]) [Text] -> IO SourceData
newSourceData infoSource =
--    debugPrint $ parseErrorMessage sd
    return sd
    where
        sourceHistory = either Just (const Nothing) infoSource
        newTextLines :: [Text]
        newTextLines = either sourceUpdatedByEvents id infoSource
        newTextHighlights = either (textHighlights . fst) (const []) infoSource
        sourceUpdatedByEvents :: (SourceData, [SourceChangeEvent]) -> [Text]
        sourceUpdatedByEvents (oldSource, events)= foldl playEventOnText (textLines oldSource) events
            where
                playEventOnText tLines (InsertText (Point px py) newLines) =
                    let (preLines, allPostLines) = splitAt py tLines
                        (currentLine, postLines) = splitAt 1 allPostLines
                        [(preText, postText)] = map (T.splitAt px) currentLine
                        newLines' = if null currentLine then newLines
                                    else let leadingText = map (T.append preText) . take 1 $ newLines
                                             (middleText, lastTextLine) = (splitAt (length newLines - 2) . drop 1 $ newLines)
                                             lastLine = map (`T.append` postText) lastTextLine
                                             singleLineText = map (`T.append` postText) leadingText
                                         in if length newLines == 1 then singleLineText else leadingText ++ middleText ++ lastLine
                    in preLines ++ newLines' ++ postLines
                playEventOnText tLines (DeleteText p@(Point px py) deletedLines) =
                    let (preLines, postLines1) = splitAt py tLines
                        (Point ex ey) = movePointByTextLines p deletedLines
                        postLines2 = drop (ey-py) postLines1
                        startLine1 = map (T.take px) . take 1 $ postLines1
                        startLine2 = map (T.drop ex) . take 1 $ postLines2
                        startLine = T.append <$> startLine1 <*> startLine2
                    in preLines ++ startLine ++ drop 1 postLines2


        oldSourceData = fmap fst sourceHistory
        sourceChangeEvents = fmap snd sourceHistory
        collectSourceInfoSpans = sort . concatMap (F.foldr (\a s -> P.srcInfoSpan a : s) [])
        srcLocToPoint (P.SrcLoc _ l c) = Point (c-1) (l-1)
        nestInfoSpans :: [P.SrcSpan] -> Forest P.SrcSpan
        nestInfoSpans [] = []
        nestInfoSpans (el:els) = Node el (nestInfoSpans inTheRangeEls) : nestInfoSpans furtherEls
            where
                (inTheRangeEls, furtherEls) = break (isAfter el) . dropWhile (el == ) $ els
                isAfter a b = (P.srcSpanEndLine b > P.srcSpanEndLine a) || ((P.srcSpanEndLine b == P.srcSpanEndLine a) && (P.srcSpanEndLine b > P.srcSpanEndLine a))
        infoSpanToBlock s = Block
                                (Point (P.srcSpanStartColumn s - 1) (P.srcSpanStartLine s - 1))
                                (Point (P.srcSpanEndColumn s - 1) (P.srcSpanEndLine s - 1))
        collectSourceBlocks = fmap (fmap infoSpanToBlock) . nestInfoSpans . collectSourceInfoSpans
        (parsedModule, errorMessage) = case ast =<< oldSourceData of
                        Nothing -> parseFullModule
                        Just oldAst -> let notChangedDecls = (filter isNotChangedDecl . allCachedDecls $ oldAst)
                                          in addStaleDeclsOnFailure $ addCachedDecls (parseModuleMinusDecls notChangedDecls) notChangedDecls
            where
                parseModuleLines textToParse = case P.parseFileContentsWithComments P.defaultParseMode . T.unpack . T.unlines $ textToParse of
                    P.ParseOk astInfo -> (Just astInfo, Nothing)
                    P.ParseFailed srcLoc msg -> (Nothing, Just (srcLoc, msg))
                parseFullModule = parseModuleLines newTextLines

                allCachedDecls (P.Module _ _ _ _ decls, _) = decls
                allCachedDecls _ = []

                isNotChangedDecl decl = maybe False (not . any eventChangesDecl) sourceChangeEvents
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
--                        eventChangesDecl (InsertChar p _) = declContainsPoint p
--                        eventChangesDecl (InsertLine p) = declContainsPoint p
                        eventChangesDecl (InsertText p _) = declContainsPoint p
                        eventChangesDecl (DeleteText p tLines) = declContainsPoint p || declContainsPoint pEnd || declIsInBetweenPoints p pEnd
                            where
                                pEnd = movePointByTextLines p tLines

                parseModuleMinusDecls decls = parseModuleLines linesToParse
                    where
                        linesToParse = foldr blankOutRange newTextLines rangesToBlankout
                        blankOutRange (P.SrcSpan _ startLine startCol endLine endCol) tLines = prefix ++ body ++ suffix
                        -- TODO: optimize this!
                            where
                                prefix = take (startLine - 1) tLines
                                body = if endLine == startLine then singleLineBody else multiLineBody
                                singleLineBody =  map (\l -> T.take (startCol - 1) l `T.append` T.pack (replicate (endCol - startCol +1) ' ') `T.append` T.drop endCol l) . take 1 . drop (startLine - 1) $ tLines
                                multiLineBody =  (map (T.take (startCol - 1)) . take 1 . drop (startLine - 1) $ tLines)
                                              ++ replicate (endLine - startLine - 1) T.empty
                                              ++ (map (\l -> T.pack (replicate endCol ' ') `T.append` T.drop endCol l) . take 1 . drop (endLine - 1) $ tLines)
                                suffix = drop endLine tLines
                        rangesToBlankout = map (P.srcInfoSpan . adjustInfoSpanByEvents . P.ann) decls
                addCachedDecls (Just (P.Module loc headers pragmas imports decls, comments), err) oldDecls = (Just (P.Module loc headers pragmas imports (updateDeclsByEvents oldDecls++decls), comments), err)
                addCachedDecls (_, err) _ = (Nothing, err)

                addStaleDeclsOnFailure (Nothing, err) = (fmap updateRangesByEvents $ ast =<< oldSourceData, err)
                    where
                        updateRangesByEvents (P.Module loc headers pragmas imports decls, comments) = (P.Module loc headers pragmas imports (updateDeclsByEvents decls), comments)
                        updateRangesByEvents m = m
                addStaleDeclsOnFailure (Just m, err) = (Just m, err)

                updateDeclsByEvents = fmap updateDecl
                    where
                        updateDecl = fmap adjustInfoSpanByEvents

                adjustInfoSpanByEvents range = maybe range (foldr adjustInfoSpanByEvent range) sourceChangeEvents
                adjustInfoSpanByEvent evt range@(P.SrcSpanInfo (P.SrcSpan file startLine startCol endLine endCol) _) =
                    if p1 == p1' && p2 == p2' then range
                    else P.noInfoSpan $ P.SrcSpan file (p1y'+1) (p1x'+1) (p2y'+1) (p2x'+1)
                    where
                        p1 = Point (startCol-1) (startLine-1)
                        p2 = Point (endCol-1) (endLine-1)
                        p1'@(Point p1x' p1y') = adjustPointByEvent evt p1
                        p2'@(Point p2x' p2y') = adjustPointByEvent evt p2

        declarations (Just (P.Module _ _ _ _ decl, _)) = decl
        declarations _ = []
        (funcDefCache, newVarDeps) = findVariableDependencies (maybe M.empty functionDefCache oldSourceData) (fmap varDeps oldSourceData) . declarations $ parsedModule
        viariablesWithIds = zip [0..] . map (\(VariableDependency v _) -> v) . concat . M.elems $ newVarDeps
        foundDepsForVariable v = concat . concat . maybeToList $ foundVarDeps (concat . M.elems $ newVarDeps) >>= Just . map (\(VariableUsage targets _) -> targets)
            where
                foundVarDeps (VariableDependency var usages:vars) = if var == v then Just usages else foundVarDeps vars
                foundVarDeps [] = Nothing
        varToId v = foundVarId viariablesWithIds
            where
                foundVarId ((idx, var):vars) = if var == v then Just idx else foundVarId vars
                foundVarId [] = Nothing
        varDepGraphNodes = map (\(idx, var) -> (var, idx, mapMaybe varToId . foundDepsForVariable $ var)) viariablesWithIds
        (newVarDepGraph, newVarDepGraphVertexToNode, newVarDepGraphKeyToVertex) = graphFromEdges varDepGraphNodes
        sd = SourceData {
            isModified = False,
            textLines = newTextLines,
            textHighlights = newTextHighlights,
            ast = parsedModule,
            parseErrorMessage = fmap (first srcLocToPoint) errorMessage,
            functionDefCache = funcDefCache,
            sourceBlocks = collectSourceBlocks $ declarations parsedModule,
            varDeps = newVarDeps,
            varDepGraph = transposeG newVarDepGraph,
            varDepGraphLookupByVertex = newVarDepGraphVertexToNode,
            varDepGraphLookupByKey = newVarDepGraphKeyToVertex,
            history = maybe [] reverse sourceChangeEvents ++ maybe [] history oldSourceData,
            undoneHistory = maybe [] undoneHistory oldSourceData
        }

