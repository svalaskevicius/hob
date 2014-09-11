module Hob.Ui.Sidebar (newSideBarFileTree, reloadSidebarTree) where

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
    customStoreSetColumn treeStore (makeColumnIdString 0) elementLabel

    col <- treeViewColumnNew

    rend <- Mv.cellRendererTextNew
    Mv.cellLayoutPackStart col rend True
    Mv.cellLayoutSetAttributes col rend treeStore (\v -> [Mv.cellText := elementLabel v])

    _ <- treeViewAppendColumn treeView col

    treeViewSetHeadersVisible treeView False
    treeViewSetModel treeView treeStore

    treeViewSetSearchColumn treeView searchCol


    _ <- treeView `on` rowCollapsed $ \ _ _ -> treeViewColumnsAutosize treeView
    _ <- treeView `on` rowActivated $ \ path _ -> activateRow =<< treeStoreGetValue treeStore path

    return ()

    where
        searchCol :: ColumnId row String
        searchCol = makeColumnIdString 0

        activateRow :: DirectoryTreeElement -> IO ()
        activateRow el = unless (isDirectory el) $ (launchFile . elementPath) el


reloadSidebarTree :: Context -> IO ()
reloadSidebarTree ctx = do
    let treeStore = fileTreeStore ctx
    let fileCtx = fileContext ctx
    let fileTreeLoader = contextFileTreeLoader fileCtx
    treeStoreClear treeStore
    treeStoreInsertForest treeStore [] 0 =<< fileTreeLoader
