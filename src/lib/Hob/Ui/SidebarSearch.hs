module Hob.Ui.SidebarSearch (
        newSideBarFileTreeSearch,
        startSidebarSearch,
        updateSidebarSearch,
        continueSidebarSearch,
        continueSidebarSearchBackwards,
        initFileTreeIndex,
    ) where

import           Data.Char                            (isPrint, toLower)
import           Data.List                            (sortBy, partition)
import           Data.Text                            (unpack)
import           Graphics.UI.Gtk
import           Graphics.UI.Gtk.General.StyleContext (styleContextAddClass,
                                                       styleContextRemoveClass)
import Control.Monad.Reader
import Data.Tree
import Data.Maybe
import qualified Data.Vector as V

import Hob.Context
import Hob.Context.UiContext
import Hob.Context.FileContext
import Hob.Control
import Hob.Ui.Sidebar        
import Hob.DirectoryTree

import Debug.Trace

{- | index based on suffix arrays -}
data LetterIndex = LetterIndex (V.Vector Char) (V.Vector Int) deriving Show
data SearchIndexNode a = PathNode LetterIndex a (SearchIndex a) | LeafNode LetterIndex a deriving Show
type SearchIndex a = [SearchIndexNode a]

type DirectorySearchIndex = SearchIndex DirectoryTreeElement

mkIndex :: String -> LetterIndex
mkIndex s = LetterIndex word indices
     where
        word = V.fromList . map toLower $ s
        indices = V.fromList . sortBy indexOrdering . take (V.length word) $ [0..]
        indexOrdering i1 i2 = let c1 = word V.! i1
                                  c2 = word V.! i2
                                  cmp = compare c1 c2
                              in if cmp == EQ then compare i1 i2 else cmp
                              
findOccurrences :: LetterIndex -> Char -> [Int]
findOccurrences (LetterIndex word idx) c = range mid
    where
        binSearch low high
          | low > high = Nothing
          | value > c = binSearch low (middle-1)
          | value < c = binSearch (middle+1) high
          | otherwise = Just middle
            where
                value = word V.! (idx V.! middle)
                middle = (low + high) `quot` 2

        mid = binSearch 0 (highBound-1)
        
        startFrom x
          | x < 0 = 0
          | otherwise = if (word V.! (idx V.! x)) == c then startFrom (x-1) else (x+1)
        
        endFrom x
          | x == highBound = highBound - 1
          | otherwise = if (word V.! (idx V.! x)) == c then endFrom (x+1) else (x-1)

        range Nothing = []
        range (Just x) = V.toList . (V.slice s (e-s+1)) $ idx
            where
                e = endFrom x
                s = startFrom x
        highBound = V.length word
                              
{- | match query against the index in return the non matching part of the query (or empty string if match was complete -}
matchQuery :: LetterIndex -> [String] -> [String]
matchQuery index@(LetterIndex word _) = match 0
    where
          match _ [] = []
          match from ([]:qs) = match from qs
          match from (q:qs)
            | from > highBound = (q:qs)
            | otherwise = if null subMatches then (q:qs)
                          else let from' = head subMatches
                               in match from' qs
                                   
                where
                    subMatches = catMaybes . map (\from' -> matchSubString from' q) $ findPossibleStarts from q

          findPossibleStarts _ [] = []
          findPossibleStarts from (c:_) = filter (>=from) $ if c == '/' then [0, highBound+1] 
                                                            else findOccurrences index c

          matchSubString from [] = Just from
          matchSubString from (c:cs) 
            | c == '/' && from == (1+highBound) && null cs = Just from
            | from > highBound = Nothing
            | c == '/' && from == 0 = matchSubString from cs 
            | (word V.! from) == c = matchSubString (from+1) cs
            | otherwise = Nothing

          highBound = V.length word - 1

buildIndex :: Forest DirectoryTreeElement -> DirectorySearchIndex
buildIndex = fmap addNode
    where
        addNode Node{rootLabel=element, subForest=children} = if isDirectory element then pathNode else leafNode
            where
                pathNode = PathNode index element (buildIndex children)
                leafNode = LeafNode index element
                index = mkIndex $ elementLabel element
                
-- findFirstMatch :: DirectorySearchIndex -> [String] -> Maybe DirectoryTreeElement
-- findFirstMatch idx = findMatch idx (const True) (const True) id

findNextMatch :: DirectorySearchIndex -> FilePath -> [String] -> Maybe DirectoryTreeElement
findNextMatch idx previous = findMatch (forwards idx) pathCheck leafCheck forwards
    where
        leafCheck element = elementPath element > previous
        cmpSubdirs a b = take (length b) a >= take (length a) b
        pathCheck element = elementPath element `cmpSubdirs` previous
        forwards = (\ (a, b)-> (a++b) ) . (partition isIndexOfDir)
            where
                isIndexOfDir (PathNode _ treeNode _) = isDirectory treeNode
                isIndexOfDir (LeafNode _ treeNode) = isDirectory treeNode

findPreviousMatch :: DirectorySearchIndex -> FilePath -> [String] -> Maybe DirectoryTreeElement
findPreviousMatch idx next = findMatch (backwards idx) pathCheck leafCheck backwards
    where
        leafCheck element = elementPath element < next
        cmpSubdirs a b = take (length b) a <= take (length a) b
        pathCheck element = elementPath element `cmpSubdirs` next
        backwards = (\ (a, b)-> (a++b) ) . (partition isIndexOfDir) . reverse
            where
                isIndexOfDir (PathNode _ treeNode _) = isDirectory treeNode
                isIndexOfDir (LeafNode _ treeNode) = isDirectory treeNode

findMatch :: DirectorySearchIndex -> (DirectoryTreeElement -> Bool) -> (DirectoryTreeElement -> Bool) -> (DirectorySearchIndex -> DirectorySearchIndex) -> [String] -> Maybe DirectoryTreeElement
findMatch index pathCheck leafCheck childrenFilter q = trace ("find: "++(show q)) $ matcher index q
    where
        mmm [] q = matcher [] q 
        mmm (x:xs) q = trace ("matching "++" "++(show q)) $ matcher (x:xs) q
        matcher [] _ = Nothing
        matcher ((LeafNode idx el):is) queries
         | null q' && leafCheck el = trace ("final match: "++(show el))$ Just el
         | otherwise = mmm is queries
         where q' = matchQuery idx queries
        matcher ((PathNode idx el childrenI):is) queries = 
            maybe (mmm is queries) Just childrenMatch
         where q' = matchQuery idx queries
               childrenMatch = if pathCheck el then 
                                    trace ("matched "++(show el)) $ mmm (childrenFilter childrenI) q'
                               else Nothing


newSideBarFileTreeSearch :: Context -> IO ()
newSideBarFileTreeSearch ctx = do
    let treeView = sidebarTree.uiContext $ ctx
    let searchEntry = sidebarTreeSearch.uiContext $ ctx
    index <- runApp ctx initFileTreeIndex
    _ <- treeView `on` keyPressEvent $ do
        key <- eventKeyVal
        maybe (return False) startSearch $ keyToChar key
    _ <- searchEntry `on` editableChanged $ runApp ctx $ updateSidebarSearch index
    _ <- searchEntry `on` focusOutEvent $ liftIO $ widgetHide searchEntry >> return False
    _ <- searchEntry `on` keyPressEvent $ do
        modifier <- eventModifier
        if Prelude.null modifier then do
            key <- eventKeyName
            case unpack key of
                "Down" -> liftIO $ runApp ctx $ continueSidebarSearch index >> return True
                "Up" -> liftIO $ runApp ctx $ continueSidebarSearchBackwards index >> return True
                "Return" -> stopSearchAndActivateResult treeView searchEntry
                _ -> return False
        else return False
    return ()
    where
        startSearch firstChar
            | isPrint firstChar = liftIO $ do
                startSidebarSearch ctx [firstChar]
                return True
            | otherwise = return False
        stopSearchAndActivateResult treeView searchEntry = liftIO $ do
            widgetGrabFocus treeView
            widgetHide searchEntry
            (path, _) <- treeViewGetCursor treeView
            column <- treeViewGetColumn treeView 0
            maybeDo (treeViewRowActivated treeView path) column
            return True

initFileTreeIndex :: App DirectorySearchIndex
initFileTreeIndex = do
    fileCtx <- fromContext fileContext
    nodeForest <- liftIO $ contextFileTreeLoader fileCtx
    return $ buildIndex nodeForest
    
startSidebarSearch :: Context -> String -> IO ()
startSidebarSearch ctx searchString = do
    let entry = sidebarTreeSearch.uiContext $ ctx
        sidebar = sidebarTree . uiContext $ ctx
        len = length searchString
    treeViewSetCursor sidebar [] Nothing
    entrySetText entry ""
    entrySetText entry searchString
    widgetShow entry
    widgetGrabFocus entry
    editableSelectRegion entry len len

updateSidebarSearch :: DirectorySearchIndex -> App ()
updateSidebarSearch index = invokeOnTreeViewAndModel continueSearch
    where
        continueSearch treeView model = do
            ctx <- ask
            let searchEntry = sidebarTreeSearch.uiContext $ ctx
            liftIO $ putStrLn "UPDATE!"
            searchString <- liftIO $ entryGetText searchEntry
            maybeFirstIter <- liftIO $ iterOnSelection treeView model
            maybeDo (selectNextMatch index model searchEntry searchString) maybeFirstIter

        iterOnSelection treeView model = do
            (path, _) <- liftIO $ treeViewGetCursor treeView
            selectedIter <- liftIO $ treeModelGetIter model path
            maybe (treeModelGetIterFirst model) (return.Just) selectedIter

continueSidebarSearch :: DirectorySearchIndex -> App ()
continueSidebarSearch index = invokeOnTreeViewAndModel continueSearch
    where
        continueSearch treeView model = do
            ctx <- ask
            let searchEntry = sidebarTreeSearch.uiContext $ ctx
            searchString <- liftIO $ entryGetText searchEntry
            (path, _) <- liftIO $ treeViewGetCursor treeView
            currentIter <- liftIO $ treeModelGetIter model path
            maybeDo (selectNextMatch index model searchEntry searchString) currentIter

continueSidebarSearchBackwards :: DirectorySearchIndex -> App ()
continueSidebarSearchBackwards index = invokeOnTreeViewAndModel continueSearch
    where
        continueSearch treeView model = do
            ctx <- ask
            let searchEntry = sidebarTreeSearch.uiContext $ ctx
            searchString <- liftIO $ entryGetText searchEntry
            (path, _) <- liftIO $ treeViewGetCursor treeView
            currentIter <- liftIO $ treeModelGetIter model path
            maybeDo (selectPreviousMatch index model searchEntry searchString) currentIter

invokeOnTreeViewAndModel :: (TreeView -> TreeModel -> App ()) -> App ()
invokeOnTreeViewAndModel fnc = do
    ctx <- ask
    let treeView = sidebarTree.uiContext $ ctx
    model <- liftIO $ treeViewGetModel treeView
    maybeDo (fnc treeView) model

selectNextMatch :: (TreeModelClass tm, EntryClass e) => DirectorySearchIndex -> tm -> e -> String -> TreeIter -> App ()
selectNextMatch index treeModel searchEntry searchString currentIter = do
    path <- liftIO $ treeModelGetValue treeModel currentIter pathColumn
    liftIO $ putStrLn $ "--- selectNextMatch-! for "++searchString++ " after " ++path
    case trace "calling findNExt Match" $ findNextMatch index path [map toLower searchString] of
        Just match -> do
            liftIO $ putStrLn $ "--- matched!" ++ (show match)
            liftIO $ unsetErrorState searchEntry 
            syncPathToSidebar $ elementPath match
        Nothing -> liftIO $ setErrorState searchEntry

selectPreviousMatch :: (TreeModelClass tm, EntryClass e) => DirectorySearchIndex -> tm -> e -> String -> TreeIter -> App ()
selectPreviousMatch index treeModel searchEntry searchString currentIter = do
    path <- liftIO $ treeModelGetValue treeModel currentIter pathColumn
    case findPreviousMatch index path [searchString] of
        Just match -> do
            liftIO $ unsetErrorState searchEntry 
            syncPathToSidebar $ elementPath match
        Nothing -> liftIO $ setErrorState searchEntry

setErrorState :: EntryClass e => e -> IO ()
setErrorState searchEntry = do
    widgetStyleContext <- widgetGetStyleContext searchEntry
    styleContextAddClass widgetStyleContext "error"

unsetErrorState :: EntryClass e => e -> IO ()
unsetErrorState searchEntry = do
    widgetStyleContext <- widgetGetStyleContext searchEntry
    styleContextRemoveClass widgetStyleContext "error"

