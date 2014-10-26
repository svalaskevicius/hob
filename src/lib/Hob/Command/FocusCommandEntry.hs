module Hob.Command.FocusCommandEntry (toggleFocusOnCommandEntryCommandHandler) where

import Graphics.UI.Gtk

import Hob.Context
import Hob.Context.UiContext
import Hob.Control
import Hob.Ui.Editor

toggleFocusOnCommandEntryCommandHandler :: CommandHandler
toggleFocusOnCommandEntryCommandHandler = CommandHandler Nothing toggleFocusOnCommandEntry

toggleFocusOnCommandEntry :: Context -> IO ()
toggleFocusOnCommandEntry ctx = do
    isFocused <- widgetGetIsFocus cmdEntry
    if isFocused then
        maybeDo widgetGrabFocus =<< getActiveEditor ctx
    else
        widgetGrabFocus cmdEntry
    where cmdEntry = commandEntry.uiContext $ ctx
