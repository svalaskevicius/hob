module Hob.Command.FocusNumberedTab (focusNumberedTabCommandHandler) where

import           Control.Monad         (when)
import           Control.Monad.Trans   (liftIO)
import           Graphics.UI.Gtk

import           Hob.Context
import           Hob.Context.UiContext

focusNumberedTabCommandHandler :: Int -> CommandHandler
focusNumberedTabCommandHandler nr = CommandHandler Nothing $ focusNumberedTab nr

focusNumberedTab :: Int -> App()
focusNumberedTab nr = do
    ui <- fromContext uiContext
    let notebook = mainNotebook ui
    pages <- liftIO $ notebookGetNPages notebook
    when (nr >= 0 && nr < pages) $ liftIO $ notebookSetCurrentPage notebook nr
