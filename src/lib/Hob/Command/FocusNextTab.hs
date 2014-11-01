module Hob.Command.FocusNextTab (focusNextTabCommandHandler) where

import qualified Control.Monad.State as S
import           Control.Monad.Trans (liftIO)
import           Graphics.UI.Gtk

import Hob.Context
import Hob.Context.UiContext

focusNextTabCommandHandler :: CommandHandler
focusNextTabCommandHandler = CommandHandler Nothing focusNextTab

focusNextTab :: Command
focusNextTab = do
    ctx <- S.get
    let notebook = mainNotebook.uiContext $ ctx
    pages <- liftIO $ notebookGetNPages notebook
    currentPage <- liftIO $ notebookGetCurrentPage notebook
    liftIO $ notebookSetCurrentPage notebook (nextPage currentPage pages)
    where
        nextPage currentPage pages =
            if currentPage == (pages - 1) then 0 else currentPage + 1

