module Hob.Ui.SidebarSearch (
        newSideBarFileTreeSearch,
        startSidebarSearch,
        updateSidebarSearch,
        continueSidebarSearch,
        continueSidebarSearchBackwards,
        initFileTreeIndex,
    ) where

import           Control.Monad.Reader
import           Data.Char                            (isPrint, toLower)
import           Data.List                            (isPrefixOf, sortBy)
import           Data.Maybe
import           Data.Text                            (unpack)
import           Data.Tree
import qualified Data.Vector                          as V
import           Graphics.UI.Gtk
import           Graphics.UI.Gtk.General.StyleContext (styleContextAddClass,
                                                       styleContextRemoveClass)

import Hob.Context
import Hob.Context.FileContext
import Hob.Context.UiContext
import Hob.Control
import Hob.DirectoryTree
import Hob.Ui.Sidebar

{-| Index based on suffix arrays

The two vectors contain the path element surrounded by slashes '/'
and a sorted symbol positions respectively. The index is generated
by a function 'mkIndex'.

Example:

@
mkIndex "example"
LetterIndex (fromList "/example/") (fromList [0,8,3,1,7,6,4,5,2])
@
-}
data LetterIndex = LetterIndex (V.Vector Char) (V.Vector Int) deriving Show
data SearchIndexNode a = PathNode LetterIndex a (SearchIndex a) | LeafNode LetterIndex a deriving Show
type SearchIndex a = [SearchIndexNode a]

instance Eq a => Eq (SearchIndexNode a) where
    (LeafNode _ a) == (LeafNode _ b) = a == b
    (PathNode _ a _) == (PathNode _ b _) = a == b
    _ == _ = False

type DirectorySearchIndex = SearchIndex DirectoryTreeElement

mkIndex :: String -> LetterIndex
mkIndex s = LetterIndex word indices
     where
        word = V.fromList . map toLower $ '/':s++"/"
        indices = V.fromList . sortBy indexOrdering . take (V.length word) $ [0..]
        indexOrdering i1 i2 = let c1 = word V.! i1
                                  c2 = word V.! i2
                                  cmp = compare c1 c2
                              in if cmp == EQ then compare i1 i2 else cmp

{-| Find all positions for a character -}
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
          | otherwise = if (word V.! (idx V.! x)) == c then startFrom (x-1) else x+1

        endFrom x
          | x == highBound = highBound - 1
          | otherwise = if (word V.! (idx V.! x)) == c then endFrom (x+1) else x-1

        range Nothing = []
        range (Just x) = V.toList . V.slice s (e-s+1) $ idx
            where
                e = endFrom x
                s = startFrom x
        highBound = V.length word

{-| Match query against the index

A query is a list for strings.
Each string is matched as a substring of the given index.
Then the next query string is matched.
Once there is a match failure, the rest of the query is returned as the result.

Example:

@
matchQuery (mkIndex "example") ["/ex", "ple"] = []
matchQuery (mkIndex "example") ["/ex", "pla"] = ["pla"]
matchQuery (mkIndex "example") ["/ex", "pla", "the rest"] = ["pla", "the rest"]
@
-}
matchQuery :: LetterIndex -> [String] -> [String]
matchQuery index@(LetterIndex word _) = match 0
    where
          match _ [] = []
          match from ([]:qs) = match from qs
          match from (q:qs)
            | from > highBound = q:qs
            | otherwise = if null subMatches then q:qs
                          else let from' = head subMatches
                               in match from' qs

                where
                    subMatches = mapMaybe (`matchSubString` q) $ findPossibleStarts from q

          findPossibleStarts _ [] = []
          findPossibleStarts from (c:_) = filter (>=from) . findOccurrences index $ c

          matchSubString from [] = Just from
          matchSubString from (c:cs)
            | from > highBound = Nothing
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

findNextMatch :: DirectorySearchIndex -> FilePath -> [String] -> Maybe DirectoryTreeElement
findNextMatch index previous = findMatch $ filterFromPath index previous

findPreviousMatch :: DirectorySearchIndex -> FilePath -> [String] -> Maybe DirectoryTreeElement
findPreviousMatch index next = findMatch $ filterFromPath (reverseIndex index) next
    where
        reverseIndex = map reverseNode . reverse
            where
                reverseNode n@(LeafNode _ _) = n
                reverseNode (PathNode letterIdx el children) = PathNode letterIdx el $ reverseIndex children

filterFromPath :: DirectorySearchIndex -> FilePath -> DirectorySearchIndex
filterFromPath [] _ = []
filterFromPath (node@(LeafNode _ el):is) path
 | elementPath el == path = node:is
 | otherwise = filterFromPath is path
filterFromPath (node@(PathNode lidx el children):is) path
 | elementPath el == path = node:is
 | (elementPath el ++ "/") `isPrefixOf` path = PathNode lidx el (filterFromPath children path) : is
 | otherwise = filterFromPath is path


findMatch :: DirectorySearchIndex -> [String] -> Maybe DirectoryTreeElement
findMatch [] _ = Nothing
findMatch (LeafNode idx el : is) queries
 | matchSuccess = Just el
 | otherwise = findMatch is queries
    where matchSuccess = null $ matchQuery idx queries
findMatch (PathNode idx _ children : is) queries = mplus childrenMatch $ findMatch is queries
    where childrenMatch = findMatch children $ matchQuery idx queries

{-| Prepare the query string

Parse the query into a query for matching.

Example:
@
prepareQueries "example" = ["example"]
prepareQueries "exam ple" = ["exam","ple"]
prepareQueries "exam/ple" = ["exam/","/ple"]
prepareQueries "exam//ple" = ["exam/","/","/ple"]
prepareQueries "exam/ ple" = ["exam/","ple"]
@
-}
prepareQueries :: String -> [String]
prepareQueries = concatMap breakSlashes . breakSpaces . map toLower
    where
        breakSpaces s = case break (== ' ') s of
                            ([], []) -> []
                            (a, []) -> [a]
                            ([], ' ':b) -> breakSpaces b
                            (a, ' ':b) -> a : breakSpaces b
                            (a, b) -> a : breakSpaces b
        breakSlashes s = case break (== '/') s of
                            ([], []) -> []
                            (a, []) -> [a]
                            ([], '/':b) -> prependToFirst '/' $ breakSlashes b
                            (a, '/':b) -> (a++"/") : prependToFirst '/' (breakSlashes b)
                            (a, b) -> (a++"/") : prependToFirst '/' (breakSlashes b)
        prependToFirst _ [] = []
        prependToFirst a ([]:xs) = [a]:xs
        prependToFirst a (x@(q:_):xs)
         | a == q = [a]:x:xs
         | otherwise = (a:x):xs

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
updateSidebarSearch index = invokeOnTreeViewAndModel $ continueSearch index selectNextMatch iterOnSelection
    where
        iterOnSelection treeView model = do
            (path, _) <- liftIO $ treeViewGetCursor treeView
            selectedIter <- liftIO $ treeModelGetIter model path
            maybe (treeModelGetIterFirst model) (return.Just) selectedIter

continueSidebarSearch :: DirectorySearchIndex -> App ()
continueSidebarSearch index = invokeOnTreeViewAndModel $ continueSearch index selectNextMatch iterAfterSelection
    where
        iterAfterSelection treeView model = do
            (path, _) <- treeViewGetCursor treeView
            currentIter <- treeModelGetIter model path
            maybe (return Nothing) (findNextSubtree model) currentIter

continueSidebarSearchBackwards :: DirectorySearchIndex -> App ()
continueSidebarSearchBackwards index = invokeOnTreeViewAndModel $ continueSearch index selectPreviousMatch iterBeforeSelection
    where
        iterBeforeSelection treeView model = do
            (path, _) <- treeViewGetCursor treeView
            currentIter <- treeModelGetIter model path
            maybe (return Nothing) (findPreviousSubtree model) currentIter


continueSearch :: t2 -> (t2 -> t3 -> Entry -> String -> a -> App()) -> (t1 -> t3 -> IO (Maybe a)) -> t1 -> t3 -> App()
continueSearch index selectMatch selectionIter treeView model = do
    ctx <- ask
    let searchEntry = sidebarTreeSearch.uiContext $ ctx
    searchString <- liftIO $ entryGetText searchEntry
    maybeFirstIter <- liftIO $ selectionIter treeView model
    maybeDo (selectMatch index model searchEntry searchString) maybeFirstIter

invokeOnTreeViewAndModel :: (TreeView -> TreeModel -> App ()) -> App ()
invokeOnTreeViewAndModel fnc = do
    ctx <- ask
    let treeView = sidebarTree.uiContext $ ctx
    model <- liftIO $ treeViewGetModel treeView
    maybeDo (fnc treeView) model

selectNextMatch :: (TreeModelClass tm, EntryClass e) => DirectorySearchIndex -> tm -> e -> String -> TreeIter -> App ()
selectNextMatch index treeModel searchEntry searchString currentIter = do
    path <- liftIO $ treeModelGetValue treeModel currentIter pathColumn
    case findNextMatch index path (prepareQueries searchString) of
        Just match -> do
            liftIO $ unsetErrorState searchEntry
            syncPathToSidebar $ elementPath match
        Nothing -> liftIO $ setErrorState searchEntry

selectPreviousMatch :: (TreeModelClass tm, EntryClass e) => DirectorySearchIndex -> tm -> e -> String -> TreeIter -> App ()
selectPreviousMatch index treeModel searchEntry searchString currentIter = do
    path <- liftIO $ treeModelGetValue treeModel currentIter pathColumn
    case findPreviousMatch index path (prepareQueries searchString) of
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

findNextSubtree :: TreeModelClass treeModel => treeModel -> TreeIter -> IO (Maybe TreeIter)
findNextSubtree model iter = do
    next <- treeModelIterNext model iter
    if isJust next then return next
    else maybe (return Nothing) (findNextSubtree model) =<< treeModelIterParent model iter

findPreviousSubtree :: TreeModelClass treeModel => treeModel -> TreeIter -> IO (Maybe TreeIter)
findPreviousSubtree model iter = do
    prev <- treeModelIterPrevious model iter
    if isJust prev then return prev
    else maybe (return Nothing) (findPreviousSubtree model) =<< treeModelIterParent model iter

treeModelIterPrevious :: TreeModelClass treeModel => treeModel -> TreeIter -> IO (Maybe TreeIter)
treeModelIterPrevious model iter = do
    parent <- treeModelIterParent model iter
    currentPath <- treeModelGetPath model iter
    let nth = last currentPath
    if nth > 0 then treeModelIterNthChild model parent (nth-1)
    else return Nothing

