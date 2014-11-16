module Hob.Command.FindText (
        searchCommandHandler,
        searchNextCommandHandler,
        searchBackwardsCommandHandler,
        getEditorSearchString,
    ) where

import qualified Control.Monad.State        as S
import           Control.Monad.Trans        (liftIO)
import           Data.Monoid                (mconcat)
import           Graphics.UI.Gtk
import           Graphics.UI.Gtk.SourceView (SourceView)

import Hob.Context
import Hob.Control
import Hob.Ui.Editor
import Hob.Ui.Editor.Search

searchCommandHandler :: String -> CommandHandler
searchCommandHandler searchText = CommandHandler (Just $ PreviewCommandHandler (searchPreview searchText) searchResetPreview) (searchStart searchText)

searchNextCommandHandler :: CommandHandler
searchNextCommandHandler = CommandHandler Nothing searchNext

searchBackwardsCommandHandler :: CommandHandler
searchBackwardsCommandHandler = CommandHandler Nothing searchPrevious

searchPreview :: String -> App()
searchPreview text = searchOnEditorCallback $ (`highlightSearchPreview` text)

searchReset :: App()
searchReset = searchOnEditorCallback $ resetSearch

searchResetPreview :: App()
searchResetPreview = searchOnEditorCallback $ resetSearchPreview

searchNext :: App()
searchNext = searchOnEditorCallback $ findNext

searchPrevious :: App()
searchPrevious = searchOnEditorCallback $ findPrevious

searchStart :: String -> App()
searchStart text = do
    searchOnEditorCallback $ (\editor -> findFirstFromCursor editor text >> widgetGrabFocus editor )
    enterMode searchMode

searchMode :: Mode
searchMode = Mode "search" matcher searchReset
    where matcher = mconcat [
                              createMatcherForKeyBinding ([Control], "Down") searchNextCommandHandler,
                              createMatcherForKeyBinding ([Control], "Up") searchBackwardsCommandHandler]

searchOnEditorCallback :: (SourceView -> IO()) -> App ()
searchOnEditorCallback searchOnEditor = do
    ctx <- S.get
    editor <- liftIO $ getActiveEditor ctx
    liftIO $ maybeDo searchOnEditor editor

