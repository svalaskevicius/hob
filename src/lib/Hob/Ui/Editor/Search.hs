module Hob.Ui.Editor.Search (
        highlightSearchPreview,
        resetSearchPreview,
        findFirstFromCursor,
        findNext,
        findPrevious,
        resetSearch,
        getEditorSearchString,
    ) where

import Data.Text                  (pack)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.SourceView (SourceView)
import System.Glib.GObject        (Quark)

import Hob.Control

highlightSearchPreview :: SourceView -> String -> IO()
highlightSearchPreview editor text = do
    buffer <- textViewGetBuffer editor
    tagTable <- textBufferGetTagTable buffer
    tag <- maybe (addNewSearchTag tagTable) return =<< textTagTableLookup tagTable "search"
    (start, end) <- textBufferGetBounds buffer
    addNewSearchTags buffer tag start end
    where
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

resetSearch :: SourceView -> IO()
resetSearch editor = do
    setEditorSearchString editor Nothing
    resetSearchPreview editor

resetSearchPreview :: SourceView -> IO()
resetSearchPreview editor = do
    buffer <- textViewGetBuffer editor
    tagTable <- textBufferGetTagTable buffer
    maybeDo (removeEditorTag buffer) =<< textTagTableLookup tagTable "search"
    where
        removeEditorTag buffer tag = do
            (start, end) <- textBufferGetBounds buffer
            textBufferRemoveTag buffer tag start end

findNext :: SourceView -> IO()
findNext editor = do
     searchString <- getEditorSearchString editor
     maybeDo (searchExecute editor) searchString

findPrevious :: SourceView -> IO()
findPrevious editor = do
    searchString <- getEditorSearchString editor
    maybeDo (searchExecuteBackwards editor) searchString

findFirstFromCursor :: SourceView -> String -> IO()
findFirstFromCursor editor text = do
    setEditorSearchString editor (Just text)
    highlightSearchPreview editor text
    searchExecute editor text

searchExecute :: SourceView -> String -> IO()
searchExecute editor text = do
    buffer <- textViewGetBuffer editor
    (_, start) <- textBufferGetSelectionBounds buffer
    maybe (retryFromStart buffer) (selectMatch editor buffer) =<< findNextResult start
    where
        findNextResult start = textIterForwardSearch start text [TextSearchTextOnly] Nothing
        retryFromStart buffer = do
            (start, _) <- textBufferGetBounds buffer
            maybeDo (selectMatch editor buffer) =<< findNextResult start

searchExecuteBackwards :: SourceView -> String -> IO()
searchExecuteBackwards editor text = do
    buffer <- textViewGetBuffer editor
    (end, _) <- textBufferGetSelectionBounds buffer
    maybe (retryFromEnd buffer) (selectMatch editor buffer) =<< findPreviousResult end
    where
        findPreviousResult end = textIterBackwardSearch end text [TextSearchTextOnly] Nothing
        retryFromEnd buffer = do
            (_, end) <- textBufferGetBounds buffer
            maybeDo (selectMatch editor buffer) =<< findPreviousResult end

selectMatch :: (TextViewClass tv, TextBufferClass tb) => tv -> tb -> (TextIter, TextIter) -> IO ()
selectMatch editor buffer (start, end) = do
    textBufferSelectRange buffer start end
    caretMark <- textBufferGetInsert buffer
    textViewScrollToMark editor caretMark 0.1 Nothing

setEditorSearchString :: SourceView -> Maybe String -> IO ()
setEditorSearchString editor searchString = do
    quark <- searchStringQuark
    objectSetAttribute quark editor searchString

getEditorSearchString :: SourceView -> IO (Maybe String)
getEditorSearchString editor = do
    quark <- searchStringQuark
    objectGetAttributeUnsafe quark editor

searchStringQuark :: IO Quark
searchStringQuark = quarkFromString "activeSearchString"
