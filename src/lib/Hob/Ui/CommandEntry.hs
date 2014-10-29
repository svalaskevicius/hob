module Hob.Ui.CommandEntry (newCommandEntry, newCommandEntryDetached) where

import           Control.Monad.Trans                  (liftIO)
import           Data.IORef
import           Data.Text                            (unpack)
import           Graphics.UI.Gtk
import qualified Graphics.UI.Gtk.General.StyleContext as GtkSc

import Hob.Context
import Hob.Control

type PreviewResetState = (PreviewCommandHandler -> IO(), Context -> IO())
commandPreviewPreviewState :: IO PreviewResetState
commandPreviewPreviewState = do
    state <- newIORef Nothing
    return (
        writeIORef state . Just,
        \ctx -> do
            resetCommand <- readIORef state
            maybeDo (\cmd -> previewReset cmd ctx >> return()) resetCommand
            writeIORef state Nothing
        )

newCommandEntry :: Context -> Entry -> CommandMatcher -> IO ()
newCommandEntry ctx cmdEntry cmdMatcher = do
    (onChange, onReturn) <- newCommandEntryDetached ctx cmdEntry cmdMatcher
    _ <- cmdEntry `on` editableChanged $ onChange
    _ <- cmdEntry `on` keyPressEvent $ do
        modifier <- eventModifier
        key <- eventKeyName
        case (modifier, unpack key) of
            ([], "Return") -> liftIO onReturn
            _ -> return False
    return ()

newCommandEntryDetached :: Context -> Entry -> CommandMatcher -> IO (IO (), IO Bool)
newCommandEntryDetached ctx cmdEntry cmdMatcher = do
    previewResetState <- commandPreviewPreviewState
    return (onChanged previewResetState, onReturn previewResetState)
    where
        onChanged =
            previewCmd ctx cmdEntry cmdMatcher
        onReturn previewResetState = do
            runCmd ctx cmdEntry cmdMatcher previewResetState
            entrySetText cmdEntry ""
            return True


previewCmd :: Context -> Entry -> CommandMatcher -> PreviewResetState -> IO ()
previewCmd ctx cmdEntry cmdMatcher (setLastPreviewCmd, dispatchLastPreviewReset) = do
    dispatchLastPreviewReset ctx
    text <- entryGetText cmdEntry
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
            _ <- previewExecute prev ctx
            setLastPreviewCmd prev

runCmd :: Context -> Entry -> CommandMatcher -> PreviewResetState -> IO ()
runCmd ctx cmdEntry cmdMatcher (_, dispatchLastPreviewReset) = do
    dispatchLastPreviewReset ctx
    text <- entryGetText cmdEntry
    let command = matchCommand cmdMatcher text
    maybeDo (\cmd -> commandExecute cmd ctx >> return()) command

setErrorStatus :: Entry -> IO ()
setErrorStatus cmdEntry = do
    cmdEntryStyleContext <- widgetGetStyleContext cmdEntry
    GtkSc.styleContextAddClass cmdEntryStyleContext "error"

setOkStatus :: Entry -> IO ()
setOkStatus cmdEntry = do
    cmdEntryStyleContext <- widgetGetStyleContext cmdEntry
    GtkSc.styleContextRemoveClass cmdEntryStyleContext "error"
