module Hob.Command.FocusCommandEntry (toggleFocusOnCommandEntryCommandHandler) where

import qualified Control.Monad.State as S
import           Control.Monad.Trans (liftIO)
import           Graphics.UI.Gtk

import Hob.Context
import Hob.Context.UiContext
import Hob.Control
import Hob.Ui.Editor

toggleFocusOnCommandEntryCommandHandler :: CommandHandler
toggleFocusOnCommandEntryCommandHandler = CommandHandler Nothing toggleFocusOnCommandEntry

toggleFocusOnCommandEntry :: Command
toggleFocusOnCommandEntry = do
    ctx <- S.get
    isFocused <- liftIO $ widgetGetIsFocus $ cmdEntry ctx
    if isFocused then do
        editor <- liftIO $ getActiveEditor ctx
        liftIO $ maybeDo widgetGrabFocus editor
    else
        liftIO $ widgetGrabFocus $ cmdEntry ctx
    where cmdEntry ctx = commandEntry.uiContext $ ctx
