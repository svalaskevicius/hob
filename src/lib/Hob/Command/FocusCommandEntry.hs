module Hob.Command.FocusCommandEntry (
    toggleFocusOnCommandEntryCommandHandler,
    focusActiveEditorAndExitLastModeCommandHandler,
    focusCommandEntryCommandHandler,
    ) where

import           Control.Monad.Reader
import           Graphics.UI.Gtk

import           Hob.Context
import           Hob.Context.UiContext
import           Hob.Control
import           Hob.Ui.Editor
import           Hob.Ui.Editor.Fancy

focusCommandEntryCommandHandler :: CommandHandler
focusCommandEntryCommandHandler = CommandHandler Nothing focusCommandEntry

focusActiveEditorAndExitLastModeCommandHandler :: CommandHandler
focusActiveEditorAndExitLastModeCommandHandler = CommandHandler Nothing focusActiveEditorAndExitLastMode

toggleFocusOnCommandEntryCommandHandler :: CommandHandler
toggleFocusOnCommandEntryCommandHandler = CommandHandler Nothing toggleFocusOnCommandEntry


focusCommandEntry :: App()
focusCommandEntry = do
    ctx <- ask
    liftIO $ widgetGrabFocus $ cmdEntry ctx
    where cmdEntry ctx = commandEntry.uiContext $ ctx

focusActiveEditorAndExitLastMode :: App()
focusActiveEditorAndExitLastMode = do
    exitLastMode
    ctx <- ask
    editor <- liftIO $ getActiveEditorWidget ctx
    liftIO $ maybeDo widgetGrabFocus editor

toggleFocusOnCommandEntry :: App()
toggleFocusOnCommandEntry = do
    ctx <- ask
    isFocused <- liftIO $ widgetGetIsFocus $ cmdEntry ctx
    if isFocused then do
        editor <- liftIO $ getActiveEditorWidget ctx
        liftIO $ maybeDo widgetGrabFocus editor
    else
        liftIO $ widgetGrabFocus $ cmdEntry ctx
    where cmdEntry ctx = commandEntry.uiContext $ ctx
