module Hob.Command.FindText (
        searchCommandHandler,
        searchNextCommandHandler,
        searchBackwardsCommandHandler,
        searchPreview,
        searchResetPreview,
        searchMode,
    ) where

import           Data.Monoid          (mconcat)
import           Graphics.UI.Gtk

import           Hob.Context
import           Hob.Ui.Editor
import           Hob.Ui.Editor.Search

searchCommandHandler :: String -> CommandHandler
searchCommandHandler searchText = CommandHandler (Just $ PreviewCommandHandler (searchPreview searchText) searchResetPreview) (searchStart searchText)

searchNextCommandHandler :: CommandHandler
searchNextCommandHandler = CommandHandler Nothing searchNext

searchBackwardsCommandHandler :: CommandHandler
searchBackwardsCommandHandler = CommandHandler Nothing searchPrevious

searchPreview :: String -> App()
searchPreview text = invokeOnActiveEditor (`highlightSearchPreview` text)

searchReset :: App()
searchReset = invokeOnActiveEditor resetSearch

searchResetPreview :: App()
searchResetPreview = invokeOnActiveEditor resetSearchPreview

searchNext :: App()
searchNext = invokeOnActiveEditor findNext

searchPrevious :: App()
searchPrevious = invokeOnActiveEditor findPrevious

searchStart :: String -> App()
searchStart text = do
    enterMode searchMode
    invokeOnActiveEditor $ \editor -> do
        findFirstFromCursor editor text
        widgetGrabFocus editor

searchMode :: Mode
searchMode = Mode "search" matcher searchReset
    where matcher = mconcat [
                              createMatcherForKeyBinding ([Control], "Down") searchNextCommandHandler,
                              createMatcherForKeyBinding ([Control], "Up") searchBackwardsCommandHandler]


