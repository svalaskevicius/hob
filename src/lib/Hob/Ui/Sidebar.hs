module Hob.Ui.Sidebar (
    newSideBarFileTree,
    reloadSidebarTree,
    activateSidebarPath,
    syncPathToSidebar,
    nameColumn,
    pathColumn
    ) where

import Control.Monad.Reader
import Data.List                 (isPrefixOf)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as Mv
import GtkExtras.LargeTreeStore  as LTS

import Hob.Command.NewTab
import Hob.Context
import Hob.Context.FileContext
import Hob.Context.UiContext
import Hob.Control
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
    _ <- treeView `on` rowActivated $ \ path _ -> activateRow path =<< LTS.treeStoreGetValue treeStore path

    return ()

    where
        initNameColumn treeStore = do
            customStoreSetColumn treeStore nameColumn elementLabel
            customStoreSetColumn treeStore pathColumn elementPath

            col <- treeViewColumnNew

            rend <- Mv.cellRendererTextNew
            Mv.cellLayoutPackStart col rend True
            Mv.cellLayoutSetAttributes col rend treeStore (\v -> [Mv.cellText := elementLabel v])

            _ <- treeViewAppendColumn treeView col
            return ()

        activateRow :: TreePath -> DirectoryTreeElement -> IO ()
        activateRow path el = if isDirectory el then void $ treeViewExpandRow treeView path False
                              else launchFile . elementPath $ el


nameColumn :: ColumnId row String
nameColumn = makeColumnIdString 0

pathColumn :: ColumnId row FilePath
pathColumn = makeColumnIdString 1

reloadSidebarTree :: Context -> IO ()
reloadSidebarTree ctx = do
    let treeStore = fileTreeStore ctx
    let fileCtx = fileContext ctx
    let fileTreeLoader = contextFileTreeLoader fileCtx
    LTS.treeStoreClear treeStore
    LTS.treeStoreInsertForest treeStore [] 0 =<< fileTreeLoader


activateSidebarPath :: TreeViewClass tv => tv -> TreePath -> IO ()
activateSidebarPath treeView path = do
    treeViewCollapseAll treeView
    treeViewExpandToPath treeView path
    treeViewSetCursor treeView path Nothing




syncPathToSidebar :: FilePath -> App()
syncPathToSidebar filePath = do
    ctx <- ask
    let treeView = sidebarTree.uiContext $ ctx
    liftIO (maybeDo (syncTreeViewModel treeView) =<< treeViewGetModel treeView)
    where
        syncTreeViewModel treeView model = do
            mStartIter <- treeModelGetIterFirst model
            maybeDo (syncToMatchingPath treeView model) mStartIter
        syncToMatchingPath treeView model startingIter =
            maybeDo (syncToIter treeView model) =<< findFilePath model filePath startingIter
        syncToIter treeView model iter = activateSidebarPath treeView =<< treeModelGetPath model iter

findFilePath :: TreeModelClass self =>
                self -> FilePath -> TreeIter -> IO (Maybe TreeIter)
findFilePath model filePath iter = do
        path <- treeModelGetValue model iter pathColumn
        if path == filePath then return $ Just iter
        else if (path++"/") `isPrefixOf` filePath then matchChildren
        else matchNextSibling
    where matchChildren = recurseToMaybeIter =<< treeModelIterChildren model iter
          matchNextSibling = recurseToMaybeIter =<< treeModelIterNext model iter
          recurseToMaybeIter Nothing = return Nothing
          recurseToMaybeIter (Just it) = findFilePath model filePath it


