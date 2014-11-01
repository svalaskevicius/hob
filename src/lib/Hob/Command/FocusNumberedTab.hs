module Hob.Command.FocusNumberedTab (focusNumberedTabCommandHandler) where

import Control.Monad   (when)
import Graphics.UI.Gtk
import qualified Control.Monad.State as S
import           Control.Monad.Trans                  (liftIO)

import Hob.Context
import Hob.Context.UiContext

focusNumberedTabCommandHandler :: Int -> CommandHandler
focusNumberedTabCommandHandler nr = CommandHandler Nothing $ focusNumberedTab nr

focusNumberedTab :: Int -> Command
focusNumberedTab nr = do
    ctx <- S.get
    let notebook = mainNotebook.uiContext $ ctx
    pages <- liftIO $ notebookGetNPages notebook
    when (nr >= 0 && nr < pages) $ liftIO $ notebookSetCurrentPage notebook nr
