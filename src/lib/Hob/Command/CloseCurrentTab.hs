module Hob.Command.CloseCurrentTab (closeCurrentEditorTab) where


import Graphics.UI.Gtk

import Hob.Command
import Hob.Context

closeCurrentEditorTab :: CommandHandler
closeCurrentEditorTab = CommandHandler Nothing closeCurrentEditorTabHandler

closeCurrentEditorTabHandler :: Context -> IO ()
closeCurrentEditorTabHandler ctx = do
    currentPage <- notebookGetCurrentPage tabbed
    nthPage <- notebookGetNthPage tabbed currentPage
    case nthPage of
        Just pageContents -> do
            notebookRemovePage tabbed currentPage
            widgetDestroy pageContents
        Nothing -> return ()
    where tabbed = mainNotebook ctx


