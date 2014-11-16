module Hob.Ui (loadGui,
               getActiveEditorText,
               getActiveEditorTab,
               getEditorText,
               getActiveEditor) where

import qualified Control.Monad.State                  as S
import           Control.Monad.Trans                  (liftIO)
import           Data.Char                            (intToDigit)
import           Data.IORef
import           Data.List                            (intercalate)
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

        ctx <- initCtx builder defaultCommands
        initMainWindow ctx
        initSidebar ctx
        initCommandEntry ctx
        initActiveModesMonitor ctx
        return ctx
    where
        initCtx builder initCommands = do
            window <- builderGetObject builder castToWindow "mainWindow"
            notebook <- builderGetObject builder castToNotebook "tabbedEditArea"
            cmdEntry <- builderGetObject builder castToEntry "command"
            treeView <- builderGetObject builder castToTreeView "directoryListing"
            treeViewSearch <- builderGetObject builder castToEntry "directoryListingSearch"
            activeModesObject <- builderGetObject builder castToLabel "activeMode"
            treeModel <- LTS.treeStoreNew []
            let uiCtx = UiContext window notebook cmdEntry treeView treeViewSearch activeModesObject
            initContext styleCtx fileCtx uiCtx treeModel initCommands
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
        initCommandEntry ctx = do
            let cmdEntry = commandEntry . uiContext $ ctx
            let cmdMatcher = baseCommands ctx
            widgetSetName cmdEntry "commandEntry"
            deferredRunner ctx $ newCommandEntry cmdEntry cmdMatcher
        initMainWindow ctx = do
            let window = mainWindow . uiContext $ ctx
            let cmdMatcher = baseCommands ctx
            lastPressedKeyRef <- newIORef ""
            widgetSetName window "mainWindow"
            _ <- window `on` keyPressEvent $ do
                modifier <- eventModifier
                keyT <- eventKeyName
                let key = unpack keyT
                liftIO $ writeIORef lastPressedKeyRef key
                if key == "Control_L" then return False
                else maybe (return False)
                           (\cmd -> liftIO $ deferredRunner ctx (commandExecute cmd) >> return True) $
                           matchKeyBinding cmdMatcher (modifier, key)
            _ <- window `on` keyReleaseEvent $ do
                modifier <- eventModifier
                keyT <- eventKeyName
                lastPressedKey <- liftIO $ readIORef lastPressedKeyRef
                let key = unpack keyT
                if (lastPressedKey == key) && (key == "Control_L") then
                    maybe (return False)
                          (\cmd -> liftIO $ deferredRunner ctx (commandExecute cmd) >> return True) $
                          matchKeyBinding cmdMatcher (modifier, key)
                else return False
            return ()
        initActiveModesMonitor ctx =
            deferredRunner ctx $ registerEventHandler (Event "core.mode.change") $ do
                activeCtx <- S.get
                modes <- activeModes
                let modesUi = activeModesLabel . uiContext $ activeCtx
                let modesToString = intercalate " | " . filter (not . null) . map modeName
                liftIO $ labelSetText modesUi $ maybe "-" modesToString modes
        defaultCommands = mconcat $ [
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
                                    createMatcherForKeyBinding ([Control], "Control_L") focusCommandEntryCommandHandler,
                                    createMatcherForKeyBinding ([], "Escape") focusActiveEditorAndExitLastModeCommandHandler,
                                    -- search / replace
                                    createMatcherForKeyBinding ([Control], "Down") searchNextCommandHandler,
                                    createMatcherForKeyBinding ([Control], "Up") searchBackwardsCommandHandler,
                                    -- replace
                                    createMatcherForKeyBinding ([Shift, Control], "Down") replaceNextCommandHandler
                                ] ++
                                [
                                    -- default
                                    createMatcherForKeyBinding ([Control], [intToDigit $ (position + 1) `mod` 10]) $ focusNumberedTabCommandHandler position
                                        | position <- [0..9]
                                ]

setGtkStyle :: StyleContext -> IO ()
setGtkStyle styleCtx = do
    cssProvider <- cssProviderNew
    cssProviderLoadFromPath cssProvider $ uiTheme styleCtx
    maybe (return()) (\screen -> GtkSc.styleContextAddProviderForScreen screen cssProvider 800) =<< screenGetDefault

