module Hob.Ui (loadGui,
               getActiveEditorText,
               getActiveEditorTab,
               getEditorText,
               getActiveEditor) where

import           Control.Monad.Trans                  (liftIO)
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
import Hob.Ui.CommandEntry
import Hob.Ui.Editor
import Hob.Ui.Sidebar


loadGui :: FileContext -> StyleContext -> IO Context
loadGui fileCtx styleCtx = do
        _ <- initGUI

        builder <- loadUiBuilder
        setGtkStyle styleCtx
        let cmdMatcher = mconcat [
                            createMatcherForKeyBinding ([Control], "w") closeCurrentEditorTab,
                            createMatcherForKeyBinding ([Control], "s") saveCurrentEditorTab,
                            createMatcherForKeyBinding ([Control], "n") editNewFileCommandHandler,
                            createMatcherForKeyBinding ([], "Escape") toggleFocusOnCommandEntryCommandHandler,
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
            newCommandEntry ctx cmdEntry cmdMatcher
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

setGtkStyle :: StyleContext -> IO ()
setGtkStyle styleCtx = do
    cssProvider <- cssProviderNew
    cssProviderLoadFromPath cssProvider $ uiTheme styleCtx
    maybe (return()) (\screen -> GtkSc.styleContextAddProviderForScreen screen cssProvider 800) =<< screenGetDefault

