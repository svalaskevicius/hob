module Hob.Command.ReplaceText (
        createMatcherForReplace,
        replaceCommandHandler,
        replaceNextCommandHandler,
    ) where

import           Control.Monad              (when)
import           Graphics.UI.Gtk
import           Data.Monoid                (mconcat)
import           Graphics.UI.Gtk.SourceView (SourceView)
import           System.Glib.GObject        (Quark)

import Hob.Command.FindText
import Hob.Context
import Hob.Control
import Hob.Ui.Editor
import Hob.Ui.Editor.Search

createMatcherForReplace :: Char -> (String -> String -> CommandHandler) -> CommandMatcher
createMatcherForReplace prefix handler = CommandMatcher (const Nothing) match
    where match [] = Nothing
          match (p:ps)
           | p == prefix = matchSearchAndReplaceFromStart ps
           | otherwise = Nothing

          matchSearchAndReplaceFromStart [] = Nothing
          matchSearchAndReplaceFromStart (separator:xs) = matchSearchAndReplaceFromSearch separator xs ""

          matchSearchAndReplaceFromSearch _ [] _ = Nothing
          matchSearchAndReplaceFromSearch separator (x:xs) accumSearch
           | '\\' == x = case xs of
                            [] -> Nothing
                            (s:xss) -> if s == separator then matchSearchAndReplaceFromSearch separator xss (accumSearch++[separator])
                                       else matchSearchAndReplaceFromSearch separator xs (accumSearch++[x])
           | separator == x = matchSearchAndReplaceFromReplace separator xs accumSearch ""
           | otherwise = matchSearchAndReplaceFromSearch separator xs (accumSearch++[x])

          matchSearchAndReplaceFromReplace _ [] search accumReplace = Just $ handler search accumReplace
          matchSearchAndReplaceFromReplace separator (x:xs) search accumReplace
           | '\\' == x = case xs of
                            [] -> Just $ handler search (accumReplace++[x])
                            (s:xss) -> if s == separator then matchSearchAndReplaceFromReplace separator xss search (accumReplace++[separator])
                                       else matchSearchAndReplaceFromReplace separator xs search (accumReplace++[x])
           | separator == x = Just $ handler search accumReplace
           | otherwise = matchSearchAndReplaceFromReplace separator xs search (accumReplace++[x])

replaceCommandHandler :: String -> String -> CommandHandler
replaceCommandHandler searchText replaceText = CommandHandler (Just $ PreviewCommandHandler (searchPreview searchText) searchResetPreview) (replaceStart searchText replaceText)

replaceNextCommandHandler :: CommandHandler
replaceNextCommandHandler = CommandHandler Nothing replaceNext

replaceStart :: String -> String -> App()
replaceStart searchText replaceText = do
    invokeOnActiveEditor $ \editor -> do
        setEditorReplaceString editor (Just replaceText)
        findFirstFromCursor editor searchText
        widgetGrabFocus editor
    enterMode replaceMode

replaceNext :: App()
replaceNext = invokeOnActiveEditor $ \editor -> do
    maybeDo (replaceSelectionWith editor) =<< getEditorReplaceString editor
    findNext editor
    where
        replaceSelectionWith editor replaceText = maybeDo (replaceSearchSelectionWith editor replaceText) =<< getEditorSearchString editor
        replaceSearchSelectionWith editor replaceText searchText = do
            buffer <- textViewGetBuffer editor
            (s, e) <- textBufferGetSelectionBounds buffer
            selectedText <- textBufferGetText buffer s e False
            when (searchText == selectedText) $ do
                textBufferDelete buffer s e
                textBufferInsert buffer s replaceText

replaceReset :: App()
replaceReset = invokeOnActiveEditor $ resetSearch -- reset replace oo

replaceMode :: Mode
replaceMode = Mode "replace" matcher replaceReset
    where matcher = mconcat [ commandMatcher searchMode
                            , createMatcherForKeyBinding ([Shift, Control], "Down") replaceNextCommandHandler
                            ]

setEditorReplaceString :: SourceView -> Maybe String -> IO ()
setEditorReplaceString editor replaceString = do
    quark <- replaceStringQuark
    objectSetAttribute quark editor replaceString

getEditorReplaceString :: SourceView -> IO (Maybe String)
getEditorReplaceString editor = do
    quark <- replaceStringQuark
    objectGetAttributeUnsafe quark editor

replaceStringQuark :: IO Quark
replaceStringQuark = quarkFromString "activeReplaceString"


