module Hob.Ui.SidebarSearch (startSidebarSearch) where

import Graphics.UI.Gtk
import Data.List (isInfixOf)
import Data.Maybe (isNothing, fromJust)

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
            firstChildIter <- treeModelIterChildren model iter
            if isNothing firstChildIter then
                checkLeafMatch model iter
            else do
                selectMatch model $ fromJust firstChildIter
            maybeDo (selectMatch model) =<< treeModelIterNext model iter
        checkLeafMatch model iter = do
            value <- treeModelGetValue model iter nameColumn
            if searchString `isInfixOf` value then do
                path <- treeModelGetPath model iter
                treeViewExpandToPath treeView path
                treeViewSetCursor treeView path Nothing
            else return()