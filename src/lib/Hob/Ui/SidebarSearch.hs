module Hob.Ui.SidebarSearch (startSidebarSearch) where

import Graphics.UI.Gtk
import Data.Maybe (fromJust, isJust)

import Hob.Ui.Sidebar (nameColumn)
import Hob.Control

startSidebarSearch :: TreeView -> String -> IO Entry
startSidebarSearch treeView searchString = do
    entry <- entryNew
    widgetSetName entry "sidebarSearchEntry"
    model <- treeViewGetModel treeView
    maybeDo startSearch model
    return entry
    where
        startSearch model = do
            maybeFirstIter <- treeModelGetIterFirst model
            maybeDo (selectMatch model) maybeFirstIter
        selectMatch model iter = do
            maybePath <- findMatchingPath model searchString iter
            maybeDo updateMatchingPath maybePath
        updateMatchingPath path = do
            treeViewExpandToPath treeView path
            treeViewSetCursor treeView path Nothing
        findMatchingPath model search iter = do
            firstChildIter <- treeModelIterChildren model iter
            value <- treeModelGetValue model iter nameColumn
            let newSearchString = search `eatMatcherFrom` ('/':value)
            if isJust firstChildIter then do
                ret <- findMatchingPath model newSearchString $ fromJust firstChildIter
                maybe (findMatchingSibling model search iter) (return.Just) ret
            else do
                if newSearchString == "" then do
                    path <- treeModelGetPath model iter
                    return $ Just path
                else findMatchingSibling model search iter
        findMatchingSibling model search iter =
            maybe (return Nothing) (findMatchingPath model search) =<< treeModelIterNext model iter

        eatMatcherFrom [] _ = []
        eatMatcherFrom search [] = search
        eatMatcherFrom (s:search) (v:value)
            | s == v = eatMatcherFrom search value
            | otherwise = eatMatcherFrom (s:search) value
            
