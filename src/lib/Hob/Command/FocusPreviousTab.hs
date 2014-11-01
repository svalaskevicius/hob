module Hob.Command.FocusPreviousTab (focusPreviousTabCommandHandler) where

import Graphics.UI.Gtk
import qualified Control.Monad.State as S
import           Control.Monad.Trans                  (liftIO)

import Hob.Context
import Hob.Context.UiContext

focusPreviousTabCommandHandler :: CommandHandler
focusPreviousTabCommandHandler = CommandHandler Nothing focusPreviousTab

focusPreviousTab :: Command
focusPreviousTab = do
    ctx <- S.get
    let notebook = mainNotebook.uiContext $ ctx
    pages <- liftIO $ notebookGetNPages notebook
    currentPage <- liftIO $ notebookGetCurrentPage notebook
    liftIO $ notebookSetCurrentPage notebook (previousPage currentPage pages)
    where
        previousPage currentPage pages =
            if currentPage == 0 then pages - 1 else currentPage - 1

