module Hob.Ui.Sidebar (newSideBarFileTree, reloadSidebarTree, nameColumn) where

import Control.Monad             (unless)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as Mv


import Hob.Command.NewTab
import Hob.Context
import Hob.Context.FileContext
import Hob.DirectoryTree

newSideBarFileTree :: Context -> TreeView -> NewFileEditorLauncher -> IO ()
newSideBarFileTree ctx treeView launchFile = do
    let treeStore = fileTreeStore ctx
    reloadSidebarTree ctx
    initNameColumn treeStore

    treeViewSetHeadersVisible treeView False
    treeViewSetEnableSearch treeView False
    treeViewSetModel treeView treeStore

    _ <- treeView `on` rowCollapsed $ \ _ _ -> treeViewColumnsAutosize treeView
    _ <- treeView `on` rowActivated $ \ path _ -> activateRow =<< treeStoreGetValue treeStore path

    return ()

    where
        initNameColumn treeStore = do
            customStoreSetColumn treeStore nameColumn elementLabel

            col <- treeViewColumnNew

            rend <- Mv.cellRendererTextNew
            Mv.cellLayoutPackStart col rend True
            Mv.cellLayoutSetAttributes col rend treeStore (\v -> [Mv.cellText := elementLabel v])

            _ <- treeViewAppendColumn treeView col
            return ()

        activateRow :: DirectoryTreeElement -> IO ()
        activateRow el = unless (isDirectory el) $ (launchFile . elementPath) el


nameColumn :: ColumnId row String
nameColumn = makeColumnIdString 0

reloadSidebarTree :: Context -> IO ()
reloadSidebarTree ctx = do
    let treeStore = fileTreeStore ctx
    let fileCtx = fileContext ctx
    let fileTreeLoader = contextFileTreeLoader fileCtx
    treeStoreClear treeStore
    treeStoreInsertForest treeStore [] 0 =<< fileTreeLoader
