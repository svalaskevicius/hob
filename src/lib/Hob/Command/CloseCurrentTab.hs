module Hob.Command.CloseCurrentTab (closeCurrentEditorTab) where


import Graphics.UI.Gtk

import Hob.Context
import Hob.Context.UiContext

closeCurrentEditorTab :: CommandHandler
closeCurrentEditorTab = CommandHandler Nothing closeCurrentEditorTabHandler

closeCurrentEditorTabHandler :: Command
closeCurrentEditorTabHandler ctx = do
    currentPage <- notebookGetCurrentPage tabbed
    nthPage <- notebookGetNthPage tabbed currentPage
    case nthPage of
        Just pageContents -> do
            notebookRemovePage tabbed currentPage
            widgetDestroy pageContents
        Nothing -> return ()
    return ctx
    where tabbed = mainNotebook.uiContext $ ctx

