module Hob.Ui.SidebarSearch (
        startSidebarSearch,
        continueSidebarSearch,
        continueSidebarSearchBackwards
    ) where

import Data.List       (intercalate)
import Data.Maybe      (fromJust, isJust)
import Graphics.UI.Gtk

import Hob.Control
import Hob.Ui.Sidebar (nameColumn)

startSidebarSearch :: TreeView -> String -> IO Entry
startSidebarSearch treeView searchString = do
    entry <- entryNew
    widgetSetName entry "sidebarSearchEntry"
    entrySetText entry searchString
    model <- treeViewGetModel treeView
    maybeDo startSearch model
    return entry
    where
        startSearch model = do
            maybeFirstIter <- treeModelGetIterFirst model
            maybeDo (selectNextMatch treeView model searchString) maybeFirstIter

continueSidebarSearch :: TreeView -> Entry -> IO ()
continueSidebarSearch treeView searchEntry = do
    model <- treeViewGetModel treeView
    maybeDo continueSearch model
    where
        continueSearch model = do
            searchString <- entryGetText searchEntry
            maybeFirstIter <- iterAfterSelection model
            maybeDo (selectNextMatch treeView model searchString) maybeFirstIter

        iterAfterSelection model = do
            (path, _) <- treeViewGetCursor treeView
            currentIter <- treeModelGetIter model path
            maybe (return Nothing) (findNextSubtree model) currentIter

continueSidebarSearchBackwards :: TreeView -> Entry -> IO ()
continueSidebarSearchBackwards treeView searchEntry = do
    model <- treeViewGetModel treeView
    maybeDo continueSearch model
    where
        continueSearch model = do
            searchString <- entryGetText searchEntry
            maybeFirstIter <- iterBeforeSelection model
            maybeDo (selectPreviousMatch treeView model searchString) maybeFirstIter

        iterBeforeSelection model = do
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
