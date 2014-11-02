module Hob.Ui (loadGui,
               getActiveEditorText,
               getActiveEditorTab,
               getEditorText,
               getActiveEditor) where

import           Control.Monad.Trans                  (liftIO)
import           Data.Char                            (intToDigit)
import           Data.Monoid                          (mconcat)
import           Data.Text                            (unpack)
import           Graphics.UI.Gtk
import           Graphics.UI.Gtk.General.CssProvider
import qualified Graphics.UI.Gtk.General.StyleContext as GtkSc
import           GtkExtras.LargeTreeStore             as LTS

import Hob.Command.CloseCurrentTab
import Hob.Command.FindText
import Hob.Command.FocusCommandEntry
import Hob.Command.FocusNextTab
import Hob.Command.FocusNumberedTab
import Hob.Command.FocusPreviousTab
import Hob.Command.FocusSidebar
import Hob.Command.NewTab
import Hob.Command.ReloadSidebarTree
import Hob.Command.ReplaceText
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

        ctx <- initCtx builder defaultMode
        let commandContextRunner = deferredRunner ctx
        initMainWindow commandContextRunner ctx
        initSidebar ctx
        initCommandEntry commandContextRunner ctx
        return ctx
    where
        initCtx builder initMode = do
            window <- builderGetObject builder castToWindow "mainWindow"
            notebook <- builderGetObject builder castToNotebook "tabbedEditArea"
            cmdEntry <- builderGetObject builder castToEntry "command"
            treeView <- builderGetObject builder castToTreeView "directoryListing"
            treeViewSearch <- builderGetObject builder castToEntry "directoryListingSearch"
            treeModel <- LTS.treeStoreNew []
            let uiCtx = UiContext window notebook cmdEntry treeView treeViewSearch
            initContext styleCtx fileCtx uiCtx treeModel initMode
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
        initCommandEntry commandContextRunner ctx = do
            let cmdEntry = commandEntry . uiContext $ ctx
            let cmdMatcher = commandMatcher . head . modeStack $ ctx
            widgetSetName cmdEntry "commandEntry"
            commandContextRunner $ newCommandEntry cmdEntry cmdMatcher
        initMainWindow commandContextRunner ctx = do
            let window = mainWindow . uiContext $ ctx
            let cmdMatcher = commandMatcher . head . modeStack $ ctx
            widgetSetName window "mainWindow"
            _ <- window `on` keyPressEvent $ do
                modifier <- eventModifier
                key <- eventKeyName
                maybe (return False)
                      (\cmd -> liftIO $ commandContextRunner (commandExecute cmd) >> return True) $
                      matchKeyBinding cmdMatcher (modifier, unpack key)
            return ()
        defaultMode =
                let cmdMatcher = mconcat $ [
                                    -- default:
                                    createMatcherForKeyBinding ([Control], "w") closeCurrentEditorTab,
                                    createMatcherForKeyBinding ([Control], "s") saveCurrentEditorTab,
                                    createMatcherForKeyBinding ([Control], "n") editNewFileCommandHandler,
                                    createMatcherForKeyBinding ([Control], "r") reloadSidebarTreeCommandHandler,
                                    createMatcherForKeyBinding ([Control], "Tab") focusNextTabCommandHandler,
                                    createMatcherForKeyBinding ([Shift, Control], "Tab") focusPreviousTabCommandHandler,
                                    createMatcherForKeyBinding ([Shift, Control], "ISO_Left_Tab") focusPreviousTabCommandHandler,
                                    createMatcherForKeyBinding ([Control], "t") focusSidebarCommandHandler,
                                    createMatcherForKeyBinding ([Shift, Control], "T") syncFocusSidebarCommandHandler,
                                    createMatcherForPrefix "/" searchCommandHandler,
                                    createMatcherForReplace 's' replaceCommandHandler,

                                    -- + focus cmd on ctrl key press
                                    -- + pop mode on escape, focuses editor

                                    -- search / replace
                                    createMatcherForKeyBinding ([Control], "Down") searchNextCommandHandler,
                                    createMatcherForKeyBinding ([Control], "Up") searchBackwardsCommandHandler,
                                    -- replace
                                    createMatcherForKeyBinding ([Shift, Control], "Down") replaceNextCommandHandler,

                                    -- tbc
                                    createMatcherForKeyBinding ([], "Escape") toggleFocusOnCommandEntryCommandHandler,
                                    createMatcherForCommand "stopSearch" searchResetCommandHandler
                                ] ++
                                [
                                    -- default
                                    createMatcherForKeyBinding ([Control], [intToDigit $ (position + 1) `mod` 10]) $ focusNumberedTabCommandHandler position
                                        | position <- [0..9]
                                ]
                in Mode "" cmdMatcher

setGtkStyle :: StyleContext -> IO ()
setGtkStyle styleCtx = do
    cssProvider <- cssProviderNew
    cssProviderLoadFromPath cssProvider $ uiTheme styleCtx
    maybe (return()) (\screen -> GtkSc.styleContextAddProviderForScreen screen cssProvider 800) =<< screenGetDefault

