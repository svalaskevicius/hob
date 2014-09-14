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
import Hob.Command.FocusNextTab
import Hob.Command.FocusPreviousTab
import Hob.Command.FocusSidebar
import Hob.Command.NewTab
import Hob.Command.ReloadSidebarTree
import Hob.Command.SaveCurrentTab
import Hob.Context
import Hob.Context.FileContext
import Hob.Context.StyleContext
import Hob.Context.UiContext
import Hob.Ui.CommandEntry
import Hob.Ui.Editor
import Hob.Ui.Sidebar
import Hob.Ui.SidebarSearch

loadGui :: FileContext -> StyleContext -> IO Context
loadGui fileCtx styleCtx = do
        _ <- initGUI

        builder <- loadUiBuilder
        setGtkStyle styleCtx

        let cmdMatcher = mconcat [
                            createMatcherForKeyBinding ([Control], "w") closeCurrentEditorTab,
                            createMatcherForKeyBinding ([Control], "s") saveCurrentEditorTab,
                            createMatcherForKeyBinding ([Control], "n") editNewFileCommandHandler,
                            createMatcherForKeyBinding ([Control], "r") reloadSidebarTreeCommandHandler,
                            createMatcherForKeyBinding ([Control], "Tab") focusNextTabCommandHandler,
                            createMatcherForKeyBinding ([Shift, Control], "Tab") focusPreviousTabCommandHandler,
                            createMatcherForKeyBinding ([Shift, Control], "ISO_Left_Tab") focusPreviousTabCommandHandler,
                            createMatcherForKeyBinding ([Control], "t") focusSidebarCommandHandler,
                            createMatcherForKeyBinding ([], "Escape") toggleFocusOnCommandEntryCommandHandler,
                            createMatcherForPrefix "/" searchCommandHandler
                        ]

        ctx <- initMainWindow builder cmdMatcher
        initSidebar ctx
        initCommandEntry ctx cmdMatcher
        return ctx
    where
        loadUiBuilder = do
            builder <- builderNew
            builderAddFromFile builder $ uiFile styleCtx
            return builder
        initSidebar ctx = do
            let treeView = sidebarTree . uiContext $ ctx
            let treeViewSearch = sidebarTreeSearch . uiContext $ ctx
            widgetSetName treeView "directoryListing"
            widgetSetName treeViewSearch "sidebarSearchEntry"
            let mainEditNotebook = mainNotebook . uiContext $ ctx
            newSideBarFileTree ctx treeView $ launchNewFileEditor ctx mainEditNotebook
            newSideBarFileTreeSearch ctx
        initCommandEntry ctx cmdMatcher = do
            let cmdEntry = commandEntry . uiContext $ ctx
            widgetSetName cmdEntry "commandEntry"
            newCommandEntry ctx cmdEntry cmdMatcher
        initMainWindow builder cmdMatcher = do
            window <- builderGetObject builder castToWindow "mainWindow"
            notebook <- builderGetObject builder castToNotebook "tabbedEditArea"
            cmdEntry <- builderGetObject builder castToEntry "command"
            treeView <- builderGetObject builder castToTreeView "directoryListing"
            treeViewSearch <- builderGetObject builder castToEntry "directoryListingSearch"
            treeModel <- treeStoreNew []
            let uiCtx = UiContext window notebook cmdEntry treeView treeViewSearch
            let ctx = Context styleCtx fileCtx uiCtx treeModel
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

