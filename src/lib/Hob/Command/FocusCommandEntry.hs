module Hob.Command.FocusCommandEntry (
    toggleFocusOnCommandEntryCommandHandler,
    focusActiveEditorAndExitLastModeCommandHandler,
    focusCommandEntryCommandHandler,
    ) where

import qualified Control.Monad.State as S
import           Control.Monad.Trans (liftIO)
import           Graphics.UI.Gtk

import Hob.Context
import Hob.Context.UiContext
import Hob.Control
import Hob.Ui.Editor

focusCommandEntryCommandHandler :: CommandHandler
focusCommandEntryCommandHandler = CommandHandler Nothing focusCommandEntry

focusActiveEditorAndExitLastModeCommandHandler :: CommandHandler
focusActiveEditorAndExitLastModeCommandHandler = CommandHandler Nothing focusActiveEditorAndExitLastMode

toggleFocusOnCommandEntryCommandHandler :: CommandHandler
toggleFocusOnCommandEntryCommandHandler = CommandHandler Nothing toggleFocusOnCommandEntry


focusCommandEntry :: App()
focusCommandEntry = do
    ctx <- S.get
    liftIO $ widgetGrabFocus $ cmdEntry ctx
    where cmdEntry ctx = commandEntry.uiContext $ ctx

focusActiveEditorAndExitLastMode :: App()
focusActiveEditorAndExitLastMode = do
    exitLastMode
    ctx <- S.get
    editor <- liftIO $ getActiveEditor ctx
    liftIO $ maybeDo widgetGrabFocus editor

toggleFocusOnCommandEntry :: App()
toggleFocusOnCommandEntry = do
    ctx <- S.get
    isFocused <- liftIO $ widgetGetIsFocus $ cmdEntry ctx
    if isFocused then do
        editor <- liftIO $ getActiveEditor ctx
        liftIO $ maybeDo widgetGrabFocus editor
    else
        liftIO $ widgetGrabFocus $ cmdEntry ctx
    where cmdEntry ctx = commandEntry.uiContext $ ctx
