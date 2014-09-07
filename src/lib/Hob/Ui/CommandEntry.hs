module Hob.Ui.CommandEntry (newCommandEntry) where

import           Control.Monad                        (when)
import           Control.Monad.Trans                  (liftIO)
import           Data.IORef
import           Data.Maybe                           (fromJust, isJust,
                                                       isNothing)
import           Data.Text                            (unpack)
import           Graphics.UI.Gtk
import qualified Graphics.UI.Gtk.General.StyleContext as GtkSc

import Hob.Command
import Hob.Context
import Hob.Control

-- add command, dispatch and clear
commandPreviewPreviewState :: IO (PreviewCommandHandler -> IO(), Context -> IO())
commandPreviewPreviewState = do
    state <- newIORef Nothing
    return (
                writeIORef state . Just,
                \ctx -> do
                    resetCommand <- readIORef state
                    maybeDo (`previewReset` ctx) resetCommand
                    writeIORef state Nothing
            )

newCommandEntry :: Context -> Entry -> CommandMatcher -> IO ()
newCommandEntry ctx cmdEntry cmdMatcher = do
    cmdEntryStyleContext <- widgetGetStyleContext cmdEntry
    (setLastPreviewCmd, dispatchLastPreviewReset) <- commandPreviewPreviewState
    _ <- cmdEntry `on` editableChanged $ do
        text <- entryGetText cmdEntry
        dispatchLastPreviewReset ctx
        if text == "" then
            GtkSc.styleContextRemoveClass cmdEntryStyleContext "error"
        else do
            let command = matchCommand cmdMatcher text
            if isNothing command then
                GtkSc.styleContextAddClass cmdEntryStyleContext "error"
            else do
                GtkSc.styleContextRemoveClass cmdEntryStyleContext "error"
                let prev = commandPreview $ fromJust command
                when (isJust prev) $ do
                    setLastPreviewCmd $ fromJust prev
                    previewExecute (fromJust prev) ctx

    _ <- cmdEntry `on` keyPressEvent $ do
        modifier <- eventModifier
        key <- eventKeyName
        case (modifier, unpack key) of
            ([], "Return") -> liftIO $ do
                text <- entryGetText cmdEntry
                if text == "" then
                    GtkSc.styleContextRemoveClass cmdEntryStyleContext "error"
                else do
                    let command = matchCommand cmdMatcher text
                    if isNothing command then
                        GtkSc.styleContextAddClass cmdEntryStyleContext "error"
                    else do
                        GtkSc.styleContextRemoveClass cmdEntryStyleContext "error"
                        commandExecute (fromJust command) ctx

                return True
            _ -> return False


    return ()
