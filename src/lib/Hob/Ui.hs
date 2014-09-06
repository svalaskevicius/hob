module Hob.Ui (loadGui,
               getActiveEditorText,
               getActiveEditorTab,
               getEditorText,
               getActiveEditor) where

import           Control.Monad                        (unless, when, (<=<))
import           Control.Monad.Trans                  (liftIO)
import           Data.Maybe                           (fromJust, isJust,
                                                       isNothing)
import           Data.Text                            (Text, unpack)
import           Graphics.UI.Gtk
import           Graphics.UI.Gtk.General.CssProvider
import qualified Graphics.UI.Gtk.General.StyleContext as GtkSc
import           Graphics.UI.Gtk.ModelView            as Mv
import           Graphics.UI.Gtk.SourceView           (SourceView,
                                                       castToSourceView)

import Hob.Command
import Hob.Context
import Hob.Context.FileContext
import Hob.Context.StyleContext
import Hob.DirectoryTree

import Data.IORef
import Data.Monoid                   (mconcat)
import Hob.Command.CloseCurrentTab
import Hob.Command.FindText
import Hob.Command.FocusCommandEntry
import Hob.Command.NewTab
import Hob.Command.SaveCurrentTab

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
            initSideBarFileTree fileCtx sidebarTree $ launchNewFileEditor ctx mainEditNotebook
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


initSideBarFileTree :: FileContext -> TreeView -> NewFileEditorLauncher -> IO ()
initSideBarFileTree fileCtx treeView launchFile = do
    let fileTreeLoader = contextFileTreeLoader fileCtx
    treeModel <- treeStoreNew =<< fileTreeLoader
    customStoreSetColumn treeModel (makeColumnIdString 0) elementLabel

    col <- treeViewColumnNew

    rend <- Mv.cellRendererTextNew
    Mv.cellLayoutPackStart col rend True
    Mv.cellLayoutSetAttributes col rend treeModel (\v -> [Mv.cellText := elementLabel v])

    _ <- treeViewAppendColumn treeView col

    treeViewSetHeadersVisible treeView False
    treeViewSetModel treeView treeModel

    treeViewSetSearchColumn treeView searchCol


    _ <- treeView `on` rowCollapsed $ \ _ _ -> treeViewColumnsAutosize treeView
    _ <- treeView `on` rowActivated $ \ path _ -> activateRow =<< treeStoreGetValue treeModel path

    return ()

    where
        searchCol :: ColumnId row String
        searchCol = makeColumnIdString 0

        activateRow :: DirectoryTreeElement -> IO ()
        activateRow el = unless (isDirectory el) $ (launchFile . elementPath) el

getActiveEditorText :: Context -> IO (Maybe Text)
getActiveEditorText ctx = do
    editor <- getActiveEditor ctx
    maybe (return Nothing) ((return . Just) <=< getEditorText) editor

getEditorText :: TextViewClass a => a -> IO Text
getEditorText textEdit = do
    textBuf <- textViewGetBuffer textEdit
    get textBuf textBufferText

getActiveEditor :: Context -> IO (Maybe SourceView)
getActiveEditor = maybe (return Nothing) getEditorFromNotebookTab <=< getActiveEditorTab

getEditorFromNotebookTab :: Widget -> IO (Maybe SourceView)
getEditorFromNotebookTab currentlyActiveEditor =
    if currentlyActiveEditor `isA` gTypeScrolledWindow then do
        let textEditScroller = castToScrolledWindow currentlyActiveEditor
        textEdit <- binGetChild textEditScroller
        return $ fmap castToSourceView textEdit
    else return Nothing

getActiveEditorTab :: Context -> IO (Maybe Widget)
getActiveEditorTab ctx = do
    pageNum <- notebookGetCurrentPage tabbed
    if pageNum < 0 then
        return Nothing
    else do
        tabs <- containerGetChildren tabbed
        return $ Just $ tabs!!pageNum
    where tabbed = mainNotebook ctx

maybeDo :: (a -> IO ()) -> Maybe a -> IO ()
maybeDo = maybe (return())
