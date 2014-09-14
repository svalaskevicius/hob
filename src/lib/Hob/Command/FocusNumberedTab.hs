module Hob.Command.FocusNumberedTab (focusNumberedTabCommandHandler) where

import Control.Monad   (when)
import Graphics.UI.Gtk

import Hob.Command
import Hob.Context
import Hob.Context.UiContext

focusNumberedTabCommandHandler :: Int -> CommandHandler
focusNumberedTabCommandHandler nr = CommandHandler Nothing $ focusNumberedTab nr

focusNumberedTab :: Int -> Context -> IO ()
focusNumberedTab nr ctx = do
    let notebook = mainNotebook.uiContext $ ctx
    pages <- notebookGetNPages notebook
    when (nr >= 0 && nr < pages) $ notebookSetCurrentPage notebook nr

