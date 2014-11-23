module Hob.Ui.CommandEntry (newCommandEntry, newCommandEntryDetached) where

import           Control.Monad.Reader
import           Data.IORef
import           Data.Text                            (unpack)
import           Graphics.UI.Gtk
import qualified Graphics.UI.Gtk.General.StyleContext as GtkSc

import Hob.Context
import Hob.Control

type PreviewResetState = (PreviewCommandHandler -> App(), App())

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

newCommandEntry :: Entry -> App ()
newCommandEntry cmdEntry = do
    ctx <- ask
    (onChange, onReturn) <- newCommandEntryDetached cmdEntry
    let liftedOn a b c = liftIO $ on a b c
    _ <- cmdEntry `liftedOn` editableChanged $ deferredRunner ctx onChange
    _ <- cmdEntry `liftedOn` keyPressEvent $ do
        modifier <- eventModifier
        key <- eventKeyName
        case (modifier, unpack key) of
            ([], "Return") -> liftIO $ deferredRunner ctx onReturn >> return True
            _ -> return False
    return ()

newCommandEntryDetached :: Entry -> App (App(), App())
newCommandEntryDetached cmdEntry = do
    previewResetState <- commandPreviewResetState
    return (onChanged previewResetState, onReturn previewResetState)
    where
        onChanged =
            previewCmd cmdEntry
        onReturn previewResetState = do
            runCmd cmdEntry previewResetState
            liftIO $ entrySetText cmdEntry ""


previewCmd :: Entry -> PreviewResetState -> App ()
previewCmd cmdEntry (setLastPreviewCmd, dispatchLastPreviewReset) = do
    dispatchLastPreviewReset
    text <- liftIO $ entryGetText cmdEntry
    if text == "" then
        setOkStatus cmdEntry
    else do
        cmdMatcher <- getActiveCommands
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

runCmd :: Entry -> PreviewResetState -> App ()
runCmd cmdEntry (_, dispatchLastPreviewReset) = do
    dispatchLastPreviewReset
    text <- liftIO $ entryGetText cmdEntry
    cmdMatcher <- getActiveCommands
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
