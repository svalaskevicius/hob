module Hob.Command.FocusPreviousTab (focusPreviousTabCommandHandler) where

import Control.Monad.Trans (liftIO)
import Graphics.UI.Gtk

import Hob.Context
import Hob.Context.UiContext

focusPreviousTabCommandHandler :: CommandHandler
focusPreviousTabCommandHandler = CommandHandler Nothing focusPreviousTab

focusPreviousTab :: App()
focusPreviousTab = do
    ui <- fromContext uiContext
    let notebook = mainNotebook ui
    pages <- liftIO $ notebookGetNPages notebook
    currentPage <- liftIO $ notebookGetCurrentPage notebook
    liftIO $ notebookSetCurrentPage notebook (previousPage currentPage pages)
    where
        previousPage currentPage pages =
            if currentPage == 0 then pages - 1 else currentPage - 1

