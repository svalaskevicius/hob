module Hob.Command.FocusSidebar (
    focusSidebarCommandHandler,
    syncFocusSidebarCommandHandler
    ) where

import Graphics.UI.Gtk

import Hob.Command
import Hob.Context
import Hob.Context.UiContext
import Hob.Control
import Hob.Ui.Editor         (getActiveEditor, getEditorFilePath)
import Hob.Ui.Sidebar        (activateSidebarPath, pathColumn)

focusSidebarCommandHandler :: CommandHandler
focusSidebarCommandHandler = CommandHandler Nothing focusSidebar

syncFocusSidebarCommandHandler :: CommandHandler
syncFocusSidebarCommandHandler = CommandHandler Nothing syncFocusSidebar

focusSidebar :: Context -> IO ()
focusSidebar = widgetGrabFocus . sidebarTree . uiContext

syncActiveEditorPathToSidebar :: Context -> IO ()
syncActiveEditorPathToSidebar ctx = maybeDo syncToEditor =<< getActiveEditor ctx
    where syncToEditor editor = maybeDo syncToFilePath =<< getEditorFilePath editor
          syncToFilePath filePath = do
              let treeView = sidebarTree.uiContext $ ctx
              model <- treeViewGetModel treeView
              maybeDo (syncTreeViewModel filePath treeView) model
          syncTreeViewModel filePath treeView model =
              treeModelForeach model (\iter -> do
                      path <- treeModelGetValue model iter pathColumn
                      if path == filePath then do
                          modelPath <- treeModelGetPath model iter
                          activateSidebarPath treeView modelPath
                          return True
                      else return False
                  )

syncFocusSidebar :: Context -> IO ()
syncFocusSidebar ctx = do
    syncActiveEditorPathToSidebar ctx
    focusSidebar ctx
