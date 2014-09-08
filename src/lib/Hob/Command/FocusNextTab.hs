module Hob.Command.FocusNextTab (focusNextTabCommandHandler) where

import Graphics.UI.Gtk

import Hob.Command
import Hob.Context

focusNextTabCommandHandler :: CommandHandler
focusNextTabCommandHandler = CommandHandler Nothing focusNextTab

focusNextTab :: Context -> IO ()
focusNextTab ctx = do
    let notebook = mainNotebook ctx
    pages <- notebookGetNPages notebook
    currentPage <- notebookGetCurrentPage notebook
    notebookSetCurrentPage notebook (nextpage currentPage pages)
    where
        nextpage currentPage pages =
            if currentPage == (pages - 1) then 0 else currentPage + 1

