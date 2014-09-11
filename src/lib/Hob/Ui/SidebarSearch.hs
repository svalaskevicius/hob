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
            maybePath <- findMatchingPath model iter
            maybeDo updateMatchingPath maybePath
        updateMatchingPath path = do
            treeViewExpandToPath treeView path
            treeViewSetCursor treeView path Nothing
        findMatchingPath model iter = do
            firstChildIter <- treeModelIterChildren model iter
            if isJust firstChildIter then do
                ret <- findMatchingPath model $ fromJust firstChildIter
                maybe (findMatchingSibling model iter) (return.Just) ret
            else do
                value <- treeModelGetValue model iter nameColumn
                if searchString `isMatching` value then do
                    path <- treeModelGetPath model iter
                    return $ Just path
                else findMatchingSibling model iter
        findMatchingSibling model iter =
            maybe (return Nothing) (findMatchingPath model) =<< treeModelIterNext model iter
        isMatching [] _ = True
        isMatching _ [] = False
        isMatching (s:search) (v:value)
            | s == v = isMatching search value
            | otherwise = isMatching (s:search) value
            