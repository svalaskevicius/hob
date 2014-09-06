module Hob.Command.FocusCommandEntry (toggleFocusOnCommandEntryCommandHandler) where


import Control.Monad              ((<=<))
import Graphics.UI.Gtk
import Graphics.UI.Gtk.SourceView (SourceView, castToSourceView)

import Hob.Command
import Hob.Context


toggleFocusOnCommandEntryCommandHandler :: CommandHandler
toggleFocusOnCommandEntryCommandHandler = CommandHandler Nothing toggleFocusOnCommandEntry

toggleFocusOnCommandEntry :: Context -> IO ()
toggleFocusOnCommandEntry ctx = do
    isFocused <- widgetGetIsFocus cmdEntry
    if isFocused then
        maybeDo widgetGrabFocus =<< getActiveEditor ctx
    else
        widgetGrabFocus cmdEntry
    where cmdEntry = commandEntry ctx

getActiveEditor :: Context -> IO (Maybe SourceView)
getActiveEditor = maybe (return Nothing) getEditorFromNotebookTab <=< getActiveEditorTab

getEditorFromNotebookTab :: Widget -> IO (Maybe SourceView)
getEditorFromNotebookTab currentlyActiveEditor =
    if currentlyActiveEditor `isA` gTypeScrolledWindow then do
        let textEditScroller = castToScrolledWindow currentlyActiveEditor
        textEdit <- binGetChild textEditScroller
        return $ fmap castToSourceView textEdit
    else return Nothing

getActiveEditorTab :: Context -> IO (Maybe Widget)
getActiveEditorTab ctx = do
    pageNum <- notebookGetCurrentPage tabbed
    if pageNum < 0 then
        return Nothing
    else do
        tabs <- containerGetChildren tabbed
        return $ Just $ tabs!!pageNum
    where tabbed = mainNotebook ctx

maybeDo :: (a -> IO ()) -> Maybe a -> IO ()
maybeDo = maybe (return())
