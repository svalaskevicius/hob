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
import           Hob.Context.UiContext
import Hob.Context.Editor

searchCommandHandler :: String -> CommandHandler
searchCommandHandler searchText = CommandHandler (Just $ PreviewCommandHandler (searchPreview searchText) searchResetPreview) (searchStart searchText)

searchNextCommandHandler :: CommandHandler
searchNextCommandHandler = CommandHandler Nothing searchNext

searchBackwardsCommandHandler :: CommandHandler
searchBackwardsCommandHandler = CommandHandler Nothing searchPrevious

searchPreview :: String -> App()
searchPreview "" = searchResetPreview
searchPreview text = updateActiveEditor (flip (runOnEditor highlightSearchPreview) text)

searchReset :: App()
searchReset = updateActiveEditor $ runOnEditor resetSearch

searchResetPreview :: App()
searchResetPreview = updateActiveEditor $ runOnEditor resetSearchPreview

searchNext :: App()
searchNext = updateActiveEditor $ runOnEditor findNext

searchPrevious :: App()
searchPrevious = updateActiveEditor $ runOnEditor findPrevious

searchStart :: String -> App()
searchStart text = do
    ui <- fromContext uiContext
    let notebook = mainNotebook ui
    enterMode searchMode
    updateActiveEditor $ \editor -> do
        editor' <- runOnEditor findFirstFromCursor editor text
        (runOnEditor activateEditor editor') notebook
        return editor'

searchMode :: Mode
searchMode = Mode "search" matcher searchReset
    where matcher = mconcat [
                              createMatcherForKeyBinding ([Control], "Down") searchNextCommandHandler,
                              createMatcherForKeyBinding ([Control], "Up") searchBackwardsCommandHandler]


