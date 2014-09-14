module Hob.Command.FocusSidebar (focusSidebarCommandHandler) where

import Graphics.UI.Gtk

import Hob.Command
import Hob.Context
import Hob.Context.UiContext

focusSidebarCommandHandler :: CommandHandler
focusSidebarCommandHandler = CommandHandler Nothing focusSidebar

focusSidebar :: Context -> IO ()
focusSidebar = widgetGrabFocus . sidebarTree . uiContext
