module Hob.Ui (loadGui,
               getActiveEditorText,
               getActiveEditorTab,
               getEditorText,
               getActiveEditor) where

import           Control.Monad                        (when)
import           Control.Monad.Trans                  (liftIO)
import           Data.IORef
import           Data.Maybe                           (fromJust, isJust,
                                                       isNothing)
import           Data.Monoid                          (mconcat)
import           Data.Text                            (unpack)
import           Graphics.UI.Gtk
import           Graphics.UI.Gtk.General.CssProvider
import qualified Graphics.UI.Gtk.General.StyleContext as GtkSc

import Hob.Command
import Hob.Command.CloseCurrentTab
import Hob.Command.FindText
import Hob.Command.FocusCommandEntry
import Hob.Command.NewTab
import Hob.Command.SaveCurrentTab
import Hob.Context
import Hob.Context.FileContext
import Hob.Context.StyleContext
import Hob.Control
import Hob.Ui.Editor
import Hob.Ui.Sidebar

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


loadGui :: FileContext -> StyleContext -> IO Context
loadGui fileCtx styleCtx = do
        _ <- initGUI

        builder <- loadUiBuilder
        setGtkStyle styleCtx
        let commands = [
                           (([Control], "w"), closeCurrentEditorTab),
                           (([Control], "s"), saveCurrentEditorTab),
                           (([Control], "n"), editNewFileCommandHandler),
                           (([], "Escape"), toggleFocusOnCommandEntryCommandHandler)
                       ]
        let cmdMatcher = mconcat [
                            CommandMatcher {
                                matchKeyBinding = findCommandByShortCut commands,
                                matchCommand = const Nothing
                            },
                            createMatcherForPrefix "/" searchCommandHandler
                        ]

        ctx <- initMainWindow builder cmdMatcher
        initSidebar ctx builder
        initCommandEntry ctx builder cmdMatcher
        return ctx
    where
        loadUiBuilder = do
            builder <- builderNew
            builderAddFromFile builder $ uiFile styleCtx
            return builder
        initSidebar ctx builder = do
            sidebarTree <- builderGetObject builder castToTreeView "directoryListing"
            widgetSetName sidebarTree "directoryListing"
            mainEditNotebook <- builderGetObject builder castToNotebook "tabbedEditArea"
            newSideBarFileTree fileCtx sidebarTree $ launchNewFileEditor ctx mainEditNotebook
        initCommandEntry ctx builder cmdMatcher = do
            cmdEntry <- builderGetObject builder castToEntry "command"
            widgetSetName cmdEntry "commandEntry"
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
        initMainWindow builder cmdMatcher = do
            window <- builderGetObject builder castToWindow "mainWindow"
            notebook <- builderGetObject builder castToNotebook "tabbedEditArea"
            cmdEntry <- builderGetObject builder castToEntry "command"
            let ctx = Context styleCtx fileCtx window notebook cmdEntry
            widgetSetName window "mainWindow"
            _ <- window `on` keyPressEvent $ do
                modifier <- eventModifier
                key <- eventKeyName
                maybe (return False)
                      (\cmd -> liftIO $ commandExecute cmd ctx >> return True) $
                      matchKeyBinding cmdMatcher (modifier, unpack key)
            return ctx
        findCommandByShortCut [] _ = Nothing
        findCommandByShortCut ((s, cmd):xs) shortCut = if s == shortCut then Just cmd else findCommandByShortCut xs shortCut

setGtkStyle :: StyleContext -> IO ()
setGtkStyle styleCtx = do
    cssProvider <- cssProviderNew
    cssProviderLoadFromPath cssProvider $ uiTheme styleCtx
    maybe (return()) (\screen -> GtkSc.styleContextAddProviderForScreen screen cssProvider 800) =<< screenGetDefault

