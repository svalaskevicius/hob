module Hob.Ui.SidebarSearch (startSidebarSearch, continueSidebarSearch) where

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
            maybeFirstIter <- nextIterAfterSelection model
            maybeDo (selectNextMatch treeView model searchString) maybeFirstIter

        nextIterAfterSelection model = do
            (path, _) <- treeViewGetCursor treeView
            currentIter <- treeModelGetIter model path
            maybe (return Nothing) (nextIterAfterIter model) currentIter

        nextIterAfterIter model iter = do
            next <- treeModelIterNext model iter
            if isJust next then
                return next
            else findNextSubtree model iter

selectNextMatch :: (TreeViewClass tv, TreeModelClass tm) => tv -> tm -> String -> TreeIter -> IO ()
selectNextMatch treeView treeModel searchString currentIter = do
    maybePath <- findMatchingPath treeModel searchString currentIter
    maybeDo updateMatchingPath maybePath
    where
        updateMatchingPath path = do
            treeViewExpandToPath treeView path
            treeViewSetCursor treeView path Nothing

        findMatchingPath model search iter = do
            subtreeSearch <- eatParentMatches model search iter
            subtreeMatches <- findMatchingPathInSubtree model subtreeSearch iter
            if isJust subtreeMatches then return subtreeMatches
            else maybe (return Nothing) (findMatchingPath model search) =<< findNextSubtree model iter

        findMatchingPathInSubtree model search iter = do
            firstChildIter <- treeModelIterChildren model iter
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
            maybe (return Nothing) (findMatchingPathInSubtree model search) =<< treeModelIterNext model iter

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

        eatMatcherFrom [] _ = []
        eatMatcherFrom search [] = search
        eatMatcherFrom (s:search) (v:value)
            | s == v = eatMatcherFrom search value
            | otherwise = eatMatcherFrom (s:search) value

findNextSubtree :: TreeModelClass treeModel => treeModel -> TreeIter -> IO (Maybe TreeIter)
findNextSubtree model iter = do
    parent <- treeModelIterParent model iter
    currentPath <- treeModelGetPath model iter
    let nth = last currentPath
    subtree <- treeModelIterNthChild model parent (nth+1)
    if isJust subtree then return subtree
    else maybe (return Nothing) (findNextSubtree model) parent
