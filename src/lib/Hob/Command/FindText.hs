module Hob.Command.FindText (searchCommandHandler) where

import Control.Monad              ((<=<))
import Data.Text                  (pack)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.SourceView (SourceView, castToSourceView)

import Hob.Command
import Hob.Context
import Hob.Control

searchCommandHandler :: String -> CommandHandler
searchCommandHandler searchText = CommandHandler (Just $ PreviewCommandHandler (searchPreview searchText) searchReset) (searchExecute searchText)

searchPreview :: String -> Context -> IO ()
searchPreview text ctx =
    maybeDo updateSearchPreview =<< getActiveEditor ctx
    where
        updateSearchPreview editor = do
            buffer <- textViewGetBuffer editor
            tagTable <- textBufferGetTagTable buffer
            tag <- maybe (addNewSearchTag tagTable) return =<< textTagTableLookup tagTable "search"
            (start, end) <- textBufferGetBounds buffer
            addNewSearchTags buffer tag start end
        addNewSearchTag tagTable = do
            tag <- textTagNew $ Just $ pack "search"
            tag `set` [textTagBackground := "#707550"]
            textTagTableAdd tagTable tag
            return tag
        addNewSearchTags buffer tag start end = do
            result <- textIterForwardSearch start text [TextSearchTextOnly] (Just end)
            case result of
                Just (matchStart, matchEnd) -> do
                    textBufferApplyTag buffer tag matchStart matchEnd
                    addNewSearchTags buffer tag matchEnd end
                Nothing -> return()

searchReset :: Context -> IO ()
searchReset ctx =
    maybeDo resetSearchPreview =<< getActiveEditor ctx
    where
        resetSearchPreview editor = do
            buffer <- textViewGetBuffer editor
            tagTable <- textBufferGetTagTable buffer
            maybeDo (removeEditorTag buffer) =<< textTagTableLookup tagTable "search"
        removeEditorTag buffer tag = do
            (start, end) <- textBufferGetBounds buffer
            textBufferRemoveTag buffer tag start end

searchExecute :: String -> Context -> IO ()
searchExecute  text ctx =
    maybeDo doSearch =<< getActiveEditor ctx
    where
        doSearch editor = do
            buffer <- textViewGetBuffer editor
            (_, start) <- textBufferGetSelectionBounds buffer
            maybe (retryFromStart editor buffer) (selectMatch editor buffer) =<< findNextResult start
        findNextResult start = textIterForwardSearch start text [TextSearchTextOnly] Nothing
        selectMatch editor buffer (start, end) = do
            textBufferSelectRange buffer start end
            caretMark <- textBufferGetInsert buffer
            textViewScrollToMark editor caretMark 0.1 Nothing
        retryFromStart editor buffer = do
            (start, _) <- textBufferGetBounds buffer
            maybeDo (selectMatch editor buffer) =<< findNextResult start

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
