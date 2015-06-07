module Hob.Command.CloseCurrentTab (closeCurrentEditorTab) where


import           Control.Monad.Trans   (liftIO)
import           Graphics.UI.Gtk

import           Hob.Context
import           Hob.Context.UiContext

closeCurrentEditorTab :: CommandHandler
closeCurrentEditorTab = CommandHandler Nothing closeCurrentEditorTabHandler

closeCurrentEditorTabHandler :: App()
closeCurrentEditorTabHandler = do
    ui <- fromContext uiContext
    let tabbed = mainNotebook ui
    currentPage <- liftIO $ notebookGetCurrentPage tabbed
    nthPage <- liftIO $ notebookGetNthPage tabbed currentPage
    case nthPage of
        Just pageContents -> do
            liftIO $ notebookRemovePage tabbed currentPage
            liftIO $ widgetDestroy pageContents
        Nothing -> return ()

