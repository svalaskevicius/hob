module Hob.Command.CloseCurrentTab (closeCurrentEditorTab) where


import qualified Control.Monad.State as S
import           Control.Monad.Trans (liftIO)
import           Graphics.UI.Gtk

import Hob.Context
import Hob.Context.UiContext

closeCurrentEditorTab :: CommandHandler
closeCurrentEditorTab = CommandHandler Nothing closeCurrentEditorTabHandler

closeCurrentEditorTabHandler :: App()
closeCurrentEditorTabHandler = do
    ctx <- S.get
    let tabbed = mainNotebook.uiContext $ ctx
    currentPage <- liftIO $ notebookGetCurrentPage tabbed
    nthPage <- liftIO $ notebookGetNthPage tabbed currentPage
    case nthPage of
        Just pageContents -> do
            liftIO $ notebookRemovePage tabbed currentPage
            liftIO $ widgetDestroy pageContents
        Nothing -> return ()

