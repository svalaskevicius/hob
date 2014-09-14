module Hob.Command.FocusPreviousTab (focusPreviousTabCommandHandler) where

import Graphics.UI.Gtk

import Hob.Command
import Hob.Context
import Hob.Context.UiContext

focusPreviousTabCommandHandler :: CommandHandler
focusPreviousTabCommandHandler = CommandHandler Nothing focusPreviousTab

focusPreviousTab :: Context -> IO ()
focusPreviousTab ctx = do
    let notebook = mainNotebook.uiContext $ ctx
    pages <- notebookGetNPages notebook
    currentPage <- notebookGetCurrentPage notebook
    notebookSetCurrentPage notebook (previousPage currentPage pages)
    where
        previousPage currentPage pages =
            if currentPage == 0 then pages - 1 else currentPage - 1

