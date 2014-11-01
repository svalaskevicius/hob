module Hob.Command.FindText (
        searchCommandHandler,
        searchNextCommandHandler,
        searchBackwardsCommandHandler,
        searchResetCommandHandler,
        getEditorSearchString,
    ) where

import Data.Text                  (pack)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.SourceView (SourceView)
import System.Glib.GObject        (Quark)
import qualified Control.Monad.State as S
import           Control.Monad.Trans                  (liftIO)

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

searchPreview :: String -> Command
searchPreview text = do
    ctx <- S.get
    editor <- liftIO $ getActiveEditor ctx
    liftIO $ maybeDo updateSearchPreview editor
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

searchReset :: Command
searchReset = do
    ctx <- S.get
    editor <- liftIO $ getActiveEditor ctx
    liftIO $ maybeDo (`setEditorSearchString` Nothing) editor
    searchResetPreview
    
searchResetPreview :: Command
searchResetPreview = do
    ctx <- S.get
    editor <- liftIO $ getActiveEditor ctx
    liftIO $ maybeDo resetSearchPreview editor
    where
        resetSearchPreview editor = do
            buffer <- textViewGetBuffer editor
            tagTable <- textBufferGetTagTable buffer
            maybeDo (removeEditorTag buffer) =<< textTagTableLookup tagTable "search"
        removeEditorTag buffer tag = do
            (start, end) <- textBufferGetBounds buffer
            textBufferRemoveTag buffer tag start end

searchNext :: Command
searchNext = do
    ctx <- S.get
    editor <- liftIO $ getActiveEditor ctx
    maybeDo searchOnEditor editor
    where
        searchOnEditor editor = do
            searchString <- liftIO $ getEditorSearchString editor
            maybeDo searchExecute searchString

searchPrevious :: Command
searchPrevious = do
    ctx <- S.get
    editor <- liftIO $ getActiveEditor ctx
    maybeDo searchOnEditor editor
    where
        searchOnEditor editor = do
            searchString <- liftIO $ getEditorSearchString editor
            maybeDo searchExecuteBackwards searchString

searchStart :: String -> Command
searchStart text = do
    ctx <- S.get
    editor <- liftIO $ getActiveEditor ctx
    maybeDo searchStartOnEditor editor
    where
        searchStartOnEditor editor = do
            liftIO $ setEditorSearchString editor (Just text)
            searchPreview text
            searchExecute text
            liftIO $ widgetGrabFocus editor


searchExecute :: String -> App ()
searchExecute text = do
    ctx <- S.get
    editor <- liftIO $ getActiveEditor ctx
    liftIO $ maybeDo doSearch editor
    where
        doSearch editor = do
            buffer <- textViewGetBuffer editor
            (_, start) <- textBufferGetSelectionBounds buffer
            maybe (retryFromStart editor buffer) (selectMatch editor buffer) =<< findNextResult start
        findNextResult start = textIterForwardSearch start text [TextSearchTextOnly] Nothing
        retryFromStart editor buffer = do
            (start, _) <- textBufferGetBounds buffer
            maybeDo (selectMatch editor buffer) =<< findNextResult start

searchExecuteBackwards :: String -> App ()
searchExecuteBackwards text = do
    ctx <- S.get
    editor <- liftIO $ getActiveEditor ctx
    liftIO $ maybeDo doSearch editor
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

