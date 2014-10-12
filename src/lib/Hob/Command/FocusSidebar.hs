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
import Hob.Ui.SidebarSearch  (startSidebarSearch)

focusSidebarCommandHandler :: CommandHandler
focusSidebarCommandHandler = CommandHandler Nothing focusSidebar

syncFocusSidebarCommandHandler :: CommandHandler
syncFocusSidebarCommandHandler = CommandHandler Nothing syncFocusSidebar

focusSidebar :: Context -> IO ()
focusSidebar = widgetGrabFocus . sidebarTree . uiContext

syncActiveEditorPathToSidebar :: Context -> IO ()
syncActiveEditorPathToSidebar ctx = maybeDo syncToEditor =<< getActiveEditor ctx
    where syncToEditor editor = maybeDo syncToFilePath =<< getEditorFilePath editor
          syncToFilePath = startSidebarSearch ctx

syncFocusSidebar :: Context -> IO ()
syncFocusSidebar ctx = do
    syncActiveEditorPathToSidebar ctx
    focusSidebar ctx
