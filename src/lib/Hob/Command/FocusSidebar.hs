module Hob.Command.FocusSidebar (
    focusSidebarCommandHandler,
    syncFocusSidebarCommandHandler
    ) where

import Data.List
import Graphics.UI.Gtk
import qualified Control.Monad.State as S
import           Control.Monad.Trans                  (liftIO)

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
focusSidebar = do
    ctx <- S.get
    liftIO $ widgetGrabFocus . sidebarTree . uiContext $ ctx

syncActiveEditorPathToSidebar :: Command
syncActiveEditorPathToSidebar = do
    ctx <- S.get
    editor <- liftIO $ getActiveEditor ctx
    liftIO $ maybeDo (syncToEditor ctx) editor
    where syncToEditor ctx editor = maybeDo (syncToFilePath ctx) =<< getEditorFilePath editor
          syncToFilePath ctx filePath = do
              let treeView = sidebarTree.uiContext $ ctx
              maybeDo (syncTreeViewModel filePath treeView) =<< treeViewGetModel treeView
          syncTreeViewModel filePath treeView model = do
              mStartIter <- treeModelGetIterFirst model
              maybeDo (syncToMatchingPath treeView model filePath) mStartIter
          syncToMatchingPath treeView model filePath startingIter =
              maybeDo (syncToIter treeView model) =<< findFilePath model filePath startingIter
          syncToIter treeView model iter = activateSidebarPath treeView =<< treeModelGetPath model iter

syncFocusSidebar :: Command
syncFocusSidebar = syncActiveEditorPathToSidebar >> focusSidebar

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

