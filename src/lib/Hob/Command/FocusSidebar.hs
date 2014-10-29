module Hob.Command.FocusSidebar (
    focusSidebarCommandHandler,
    syncFocusSidebarCommandHandler
    ) where

import Data.List
import Graphics.UI.Gtk

import Hob.Context
import Hob.Context.UiContext
import Hob.Control
import Hob.Ui.Editor         (getActiveEditor, getEditorFilePath)
import Hob.Ui.Sidebar        (activateSidebarPath, pathColumn)

focusSidebarCommandHandler :: CommandHandler
focusSidebarCommandHandler = CommandHandler Nothing focusSidebar

syncFocusSidebarCommandHandler :: CommandHandler
syncFocusSidebarCommandHandler = CommandHandler Nothing syncFocusSidebar

focusSidebar :: Command
focusSidebar ctx = do
    widgetGrabFocus . sidebarTree . uiContext $ ctx
    return ctx

syncActiveEditorPathToSidebar :: Command
syncActiveEditorPathToSidebar ctx = do
    maybeDo syncToEditor =<< getActiveEditor ctx
    return ctx
    where syncToEditor editor = maybeDo syncToFilePath =<< getEditorFilePath editor
          syncToFilePath filePath = do
              let treeView = sidebarTree.uiContext $ ctx
              maybeDo (syncTreeViewModel filePath treeView) =<< treeViewGetModel treeView
          syncTreeViewModel filePath treeView model = do
              mStartIter <- treeModelGetIterFirst model
              maybeDo (syncToMatchingPath treeView model filePath) mStartIter
          syncToMatchingPath treeView model filePath startingIter =
              maybeDo (syncToIter treeView model) =<< findFilePath model filePath startingIter
          syncToIter treeView model iter = activateSidebarPath treeView =<< treeModelGetPath model iter

syncFocusSidebar :: Command
syncFocusSidebar ctx = syncActiveEditorPathToSidebar ctx >>= focusSidebar

findFilePath :: TreeModelClass self =>
                self -> FilePath -> TreeIter -> IO (Maybe TreeIter)
findFilePath model filePath iter = do
        path <- treeModelGetValue model iter pathColumn
        if path == filePath then return $ Just iter
        else if path `isPrefixOf` filePath then matchChildren
        else matchNextSibling
    where matchChildren = recurseToMaybeIter =<< treeModelIterChildren model iter
          matchNextSibling = recurseToMaybeIter =<< treeModelIterNext model iter
          recurseToMaybeIter mIter =
              case mIter of
                  Nothing -> return Nothing
                  (Just it) -> findFilePath model filePath it

