module Hob.Ui.SidebarSearch (
        startSidebarSearch,
        updateSidebarSearch,
        continueSidebarSearch,
        continueSidebarSearchBackwards
    ) where

import Data.List       (intercalate)
import Data.Maybe      (fromJust, isJust)
import Graphics.UI.Gtk

import Hob.Control
import Hob.Context

startSidebarSearch :: Context -> String -> IO ()
startSidebarSearch ctx searchString = do
    let treeView = sidebarTree ctx
    let entry = sidebarTreeSearch ctx
    widgetSetName entry "sidebarSearchEntry"
    entrySetText entry searchString
    model <- treeViewGetModel treeView
    maybeDo (startSearch treeView) model
    where
        startSearch treeView model = do
            maybeFirstIter <- treeModelGetIterFirst model
            maybeDo (selectNextMatch treeView model searchString) maybeFirstIter

updateSidebarSearch :: Context -> IO ()
updateSidebarSearch ctx = do
    let treeView = sidebarTree ctx
    model <- treeViewGetModel treeView
    maybeDo (continueSearch treeView) model
    where
        continueSearch treeView model = do
            let searchEntry = sidebarTreeSearch ctx
            searchString <- entryGetText searchEntry
            maybeFirstIter <- iterOnSelection treeView model
            maybeDo (selectNextMatch treeView model searchString) maybeFirstIter

        iterOnSelection treeView model = do
            (path, _) <- treeViewGetCursor treeView
            treeModelGetIter model path

continueSidebarSearch :: Context -> IO ()
continueSidebarSearch ctx = do
    let treeView = sidebarTree ctx
    model <- treeViewGetModel treeView
    maybeDo (continueSearch treeView) model
    where
        continueSearch treeView model = do
            let searchEntry = sidebarTreeSearch ctx
            searchString <- entryGetText searchEntry
            maybeFirstIter <- iterAfterSelection treeView model
            maybeDo (selectNextMatch treeView model searchString) maybeFirstIter

        iterAfterSelection treeView model = do
            (path, _) <- treeViewGetCursor treeView
            currentIter <- treeModelGetIter model path
            maybe (return Nothing) (findNextSubtree model) currentIter

continueSidebarSearchBackwards :: Context -> IO ()
continueSidebarSearchBackwards ctx = do
    let treeView = sidebarTree ctx
    model <- treeViewGetModel treeView
    maybeDo (continueSearch treeView) model
    where
        continueSearch treeView model = do
            let searchEntry = sidebarTreeSearch ctx
            searchString <- entryGetText searchEntry
            maybeFirstIter <- iterBeforeSelection treeView model
            maybeDo (selectPreviousMatch treeView model searchString) maybeFirstIter

        iterBeforeSelection treeView model = do
            (path, _) <- treeViewGetCursor treeView
            currentIter <- treeModelGetIter model path
            maybe (return Nothing) (findPreviousSubtree model) currentIter

selectNextMatch :: (TreeViewClass tv, TreeModelClass tm) => tv -> tm -> String -> TreeIter -> IO ()
selectNextMatch treeView treeModel =
    selectMatch (findNextSubtree treeModel) (treeModelIterChildren treeModel) (treeModelIterNext treeModel) treeView treeModel

selectPreviousMatch :: (TreeViewClass tv, TreeModelClass tm) => tv -> tm -> String -> TreeIter -> IO ()
selectPreviousMatch treeView treeModel =
    selectMatch (findPreviousSubtree treeModel) (treeModelIterLastChild treeModel) (treeModelIterPrevious treeModel) treeView treeModel

selectMatch :: (TreeViewClass tv, TreeModelClass tm) => 
                (TreeIter -> IO (Maybe TreeIter)) -> (TreeIter -> IO (Maybe TreeIter)) -> (TreeIter -> IO (Maybe TreeIter)) ->
                tv -> tm -> String -> TreeIter -> IO ()
selectMatch findNextSubTreeToMatch findFirstChildToMatch findNextChildToMatch
                treeView treeModel searchString currentIter = do
    maybePath <- findMatchingPath treeModel searchString currentIter
    maybeDo (updateMatchingPath treeView) maybePath
    where
        findMatchingPath model search iter = do
            subtreeSearch <- eatParentMatches model search iter
            subtreeMatches <- findMatchingPathInSubtree model subtreeSearch iter
            if isJust subtreeMatches then return subtreeMatches
            else maybe (return Nothing) (findMatchingPath model search) =<< findNextSubTreeToMatch iter

        findMatchingPathInSubtree model search iter = do
            firstChildIter <- findFirstChildToMatch iter
            value <- treeModelGetValue model iter nameColumn
            let newSearchString = search `eatMatcherFrom` ('/':value)
            if isJust firstChildIter then do
                ret <- findMatchingPathInSubtree model newSearchString $ fromJust firstChildIter
                maybe (findMatchingSibling model search iter) (return.Just) ret
            else
                if newSearchString == "" then do
                    path <- treeModelGetPath model iter
                    return $ Just path
                else findMatchingSibling model search iter

        findMatchingSibling model search iter =
            maybe (return Nothing) (findMatchingPathInSubtree model search) =<< findNextChildToMatch iter



updateMatchingPath :: TreeViewClass tv => tv -> TreePath -> IO ()
updateMatchingPath treeView path = do
    treeViewExpandToPath treeView path
    treeViewSetCursor treeView path Nothing

eatParentMatches :: TreeModelClass treeModel => treeModel -> String -> TreeIter -> IO String
eatParentMatches model search iter = do
    values <- getValues =<< getParents iter
    return $ eatMatcherFrom search $ '/' : intercalate "/" values
    where
        getValues = mapM (\it -> treeModelGetValue model it nameColumn)
        getParents childIter = do
            parent <- treeModelIterParent model childIter
            maybe
                (return [])
                (\p -> do
                    parents <- getParents p
                    return $ parents++[p])
                parent

eatMatcherFrom :: String -> String -> String
eatMatcherFrom [] _ = []
eatMatcherFrom search [] = search
eatMatcherFrom (s:search) (v:value)
    | s == v = eatMatcherFrom search value
    | otherwise = eatMatcherFrom (s:search) value

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
    treeModelIterNthChild model parent (nth-1)

treeModelIterLastChild :: TreeModelClass treeModel => treeModel -> TreeIter -> IO (Maybe TreeIter)
treeModelIterLastChild model iter = do
    childrenCount <- treeModelIterNChildren model $ Just iter
    treeModelIterNthChild model (Just iter) (childrenCount - 1)

nameColumn :: ColumnId row String
nameColumn = makeColumnIdString 0
