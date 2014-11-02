module Hob.Ui.CommandEntry (newCommandEntry, newCommandEntryDetached) where

import qualified Control.Monad.State                  as S
import           Control.Monad.Trans                  (liftIO)
import           Data.IORef
import           Data.Text                            (unpack)
import           Graphics.UI.Gtk
import qualified Graphics.UI.Gtk.General.StyleContext as GtkSc

import Hob.Context
import Hob.Control

type PreviewResetState = (PreviewCommandHandler -> App(), Command)

commandPreviewResetState :: App PreviewResetState
commandPreviewResetState = do
    state <- liftIO $ newIORef Nothing
    return (
        liftIO . writeIORef state . Just,
        do
            resetCommand <- liftIO $ readIORef state
            maybeDo previewReset resetCommand
            liftIO $ writeIORef state Nothing
        )

newCommandEntry :: Entry -> CommandMatcher -> App ()
newCommandEntry cmdEntry cmdMatcher = do
    ctx <- S.get
    (onChange, onReturn) <- newCommandEntryDetached cmdEntry cmdMatcher
    let liftedOn a b c = liftIO $ on a b c
    let runInCtx command = deferredRunner ctx command
    _ <- cmdEntry `liftedOn` editableChanged $ runInCtx onChange
    _ <- cmdEntry `liftedOn` keyPressEvent $ do
        modifier <- eventModifier
        key <- eventKeyName
        case (modifier, unpack key) of
            ([], "Return") -> liftIO $ runInCtx onReturn >> return True
            _ -> return False
    return ()

newCommandEntryDetached :: Entry -> CommandMatcher -> App (Command, Command)
newCommandEntryDetached cmdEntry cmdMatcher = do
    previewResetState <- commandPreviewResetState
    return (onChanged previewResetState, onReturn previewResetState)
    where
        onChanged =
            previewCmd cmdEntry cmdMatcher
        onReturn previewResetState = do
            runCmd cmdEntry cmdMatcher previewResetState
            liftIO $ entrySetText cmdEntry ""


previewCmd :: Entry -> CommandMatcher -> PreviewResetState -> App ()
previewCmd cmdEntry cmdMatcher (setLastPreviewCmd, dispatchLastPreviewReset) = do
    dispatchLastPreviewReset
    text <- liftIO $ entryGetText cmdEntry
    if text == "" then
        setOkStatus cmdEntry
    else do
        let command = matchCommand cmdMatcher text
        maybe notifyFailure handleCommand command
    where
        notifyFailure = setErrorStatus cmdEntry
        handleCommand command = do
            setOkStatus cmdEntry
            maybeDo invokePreview $ commandPreview command
        invokePreview prev = do
            previewExecute prev
            setLastPreviewCmd prev

runCmd :: Entry -> CommandMatcher -> PreviewResetState -> App ()
runCmd cmdEntry cmdMatcher (_, dispatchLastPreviewReset) = do
    dispatchLastPreviewReset
    text <- liftIO $ entryGetText cmdEntry
    let command = matchCommand cmdMatcher text
    maybeDo commandExecute command

setErrorStatus :: Entry -> App ()
setErrorStatus cmdEntry = do
    cmdEntryStyleContext <- liftIO $ widgetGetStyleContext cmdEntry
    liftIO $ GtkSc.styleContextAddClass cmdEntryStyleContext "error"

setOkStatus :: Entry -> App ()
setOkStatus cmdEntry = do
    cmdEntryStyleContext <- liftIO $ widgetGetStyleContext cmdEntry
    liftIO $ GtkSc.styleContextRemoveClass cmdEntryStyleContext "error"
