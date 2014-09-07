module Hob.Ui.Sidebar (newSideBarFileTree) where

import Control.Monad             (unless)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as Mv

import Hob.Command.NewTab
import Hob.Context.FileContext
import Hob.DirectoryTree

newSideBarFileTree :: FileContext -> TreeView -> NewFileEditorLauncher -> IO ()
newSideBarFileTree fileCtx treeView launchFile = do
    let fileTreeLoader = contextFileTreeLoader fileCtx
    treeModel <- treeStoreNew =<< fileTreeLoader
    customStoreSetColumn treeModel (makeColumnIdString 0) elementLabel

    col <- treeViewColumnNew

    rend <- Mv.cellRendererTextNew
    Mv.cellLayoutPackStart col rend True
    Mv.cellLayoutSetAttributes col rend treeModel (\v -> [Mv.cellText := elementLabel v])

    _ <- treeViewAppendColumn treeView col

    treeViewSetHeadersVisible treeView False
    treeViewSetModel treeView treeModel

    treeViewSetSearchColumn treeView searchCol


    _ <- treeView `on` rowCollapsed $ \ _ _ -> treeViewColumnsAutosize treeView
    _ <- treeView `on` rowActivated $ \ path _ -> activateRow =<< treeStoreGetValue treeModel path

    return ()

    where
        searchCol :: ColumnId row String
        searchCol = makeColumnIdString 0

        activateRow :: DirectoryTreeElement -> IO ()
        activateRow el = unless (isDirectory el) $ (launchFile . elementPath) el
