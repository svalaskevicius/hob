module Hob.Command.FindText (
        searchCommandHandler,
        searchNextCommandHandler,
        searchBackwardsCommandHandler,
        searchResetCommandHandler
    ) where

import Control.Monad              ((<=<))
import Data.Text                  (pack)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.SourceView (SourceView)
import System.Glib.GObject        (Quark)

import Hob.Command
import Hob.Context
import Hob.Control
import Hob.Ui.Editor

searchCommandHandler :: String -> CommandHandler
searchCommandHandler searchText = CommandHandler (Just $ PreviewCommandHandler (searchPreview searchText) searchResetPreview) (searchStart searchText)

searchNextCommandHandler :: CommandHandler
searchNextCommandHandler = CommandHandler Nothing searchNext

searchBackwardsCommandHandler :: CommandHandler
searchBackwardsCommandHandler = CommandHandler Nothing searchPrevious

searchResetCommandHandler :: CommandHandler
searchResetCommandHandler = CommandHandler Nothing searchReset

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
searchReset ctx = do
    maybeDo (`setEditorSearchString` Nothing) =<< getActiveEditor ctx
    searchResetPreview ctx

searchResetPreview :: Context -> IO ()
searchResetPreview ctx = maybeDo resetSearchPreview =<< getActiveEditor ctx
    where
        resetSearchPreview editor = do
            buffer <- textViewGetBuffer editor
            tagTable <- textBufferGetTagTable buffer
            maybeDo (removeEditorTag buffer) =<< textTagTableLookup tagTable "search"
        removeEditorTag buffer tag = do
            (start, end) <- textBufferGetBounds buffer
            textBufferRemoveTag buffer tag start end

searchNext :: Context -> IO ()
searchNext ctx = maybeDo searchOnEditor =<< getActiveEditor ctx
    where
        searchOnEditor = maybeDo (`searchExecute` ctx) <=< getEditorSearchString

searchPrevious :: Context -> IO ()
searchPrevious ctx = maybeDo searchOnEditor =<< getActiveEditor ctx
    where
        searchOnEditor = maybeDo (`searchExecuteBackwards` ctx) <=< getEditorSearchString

searchStart :: String -> Context -> IO ()
searchStart text ctx = maybeDo searchStartOnEditor =<< getActiveEditor ctx
    where
        searchStartOnEditor editor = do
            setEditorSearchString editor (Just text)
            searchPreview text ctx
            searchExecute text ctx


searchExecute :: String -> Context -> IO ()
searchExecute text ctx =
    maybeDo doSearch =<< getActiveEditor ctx
    where
        doSearch editor = do
            buffer <- textViewGetBuffer editor
            (_, start) <- textBufferGetSelectionBounds buffer
            maybe (retryFromStart editor buffer) (selectMatch editor buffer) =<< findNextResult start
        findNextResult start = textIterForwardSearch start text [TextSearchTextOnly] Nothing
        retryFromStart editor buffer = do
            (start, _) <- textBufferGetBounds buffer
            maybeDo (selectMatch editor buffer) =<< findNextResult start

searchExecuteBackwards :: String -> Context -> IO ()
searchExecuteBackwards text ctx =
    maybeDo doSearch =<< getActiveEditor ctx
    where
        doSearch editor = do
            buffer <- textViewGetBuffer editor
            (end, _) <- textBufferGetSelectionBounds buffer
            maybe (retryFromEnd editor buffer) (selectMatch editor buffer) =<< findPreviousResult end
        findPreviousResult end = textIterBackwardSearch end text [TextSearchTextOnly] Nothing
        retryFromEnd editor buffer = do
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

