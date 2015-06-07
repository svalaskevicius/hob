module Hob.Command.FocusSidebar (
    focusSidebarCommandHandler,
    syncFocusSidebarCommandHandler
    ) where

import           Control.Monad.Reader
import           Graphics.UI.Gtk

import           Hob.Context
import           Hob.Context.Editor
import           Hob.Context.UiContext
import           Hob.Control
import           Hob.Ui.Sidebar        (syncPathToSidebar)

focusSidebarCommandHandler :: CommandHandler
focusSidebarCommandHandler = CommandHandler Nothing focusSidebar

syncFocusSidebarCommandHandler :: CommandHandler
syncFocusSidebarCommandHandler = CommandHandler Nothing syncFocusSidebar

focusSidebar :: App()
focusSidebar = do
    ctx <- ask
    liftIO $ widgetGrabFocus . sidebarTree . uiContext $ ctx

syncActiveEditorPathToSidebar :: App()
syncActiveEditorPathToSidebar = do
    ctx <- ask
    editor <- currentEditor
    maybeDo syncToEditor editor
    where syncToEditor editor = maybeDo syncPathToSidebar =<< getEditorFilePath editor editor


syncFocusSidebar :: App()
syncFocusSidebar = syncActiveEditorPathToSidebar >> focusSidebar


