module Hob.Command.FocusNextTab (focusNextTabCommandHandler) where

import Graphics.UI.Gtk

import Hob.Context
import Hob.Context.UiContext

focusNextTabCommandHandler :: CommandHandler
focusNextTabCommandHandler = CommandHandler Nothing focusNextTab

focusNextTab :: Context -> IO ()
focusNextTab ctx = do
    let notebook = mainNotebook.uiContext $ ctx
    pages <- notebookGetNPages notebook
    currentPage <- notebookGetCurrentPage notebook
    notebookSetCurrentPage notebook (nextPage currentPage pages)
    where
        nextPage currentPage pages =
            if currentPage == (pages - 1) then 0 else currentPage + 1

