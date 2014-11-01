module Hob.Command.ReplaceText (
        createMatcherForReplace,
        replaceCommandHandler,
        replaceNextCommandHandler,
        generateReplaceCommandHandler,
        generateReplaceNextCommandHandler,
    ) where

import           Control.Monad              (when)
import qualified Control.Monad.State        as S
import           Control.Monad.Trans        (liftIO)
import           Data.Maybe                 (fromJust)
import           Graphics.UI.Gtk
import           Graphics.UI.Gtk.SourceView (SourceView)
import           System.Glib.GObject        (Quark)

import Hob.Command.FindText
import Hob.Context
import Hob.Control
import Hob.Ui.Editor

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

generateReplaceCommandHandler :: (String -> PreviewCommandHandler) -> (String -> Command) -> String -> String -> CommandHandler
generateReplaceCommandHandler previewCmdHandler decoratedCmdHandler searchText replaceText =
    CommandHandler (Just $ previewCmdHandler searchText) executeHandler
    where executeHandler = decoratedCmdHandler searchText >> replaceStart searchText replaceText

generateReplaceNextCommandHandler :: Command -> CommandHandler
generateReplaceNextCommandHandler decoratedCmdHandler = CommandHandler Nothing executeHandler
    where executeHandler = replaceBeforeNext >> decoratedCmdHandler

replaceCommandHandler :: String -> String -> CommandHandler
replaceCommandHandler = generateReplaceCommandHandler
                            (fromJust . commandPreview . searchCommandHandler)
                            (commandExecute . searchCommandHandler)

replaceNextCommandHandler :: CommandHandler
replaceNextCommandHandler = generateReplaceNextCommandHandler (commandExecute searchNextCommandHandler)

replaceStart :: String -> String -> Command
replaceStart _ replaceText = do
    ctx <- S.get
    editor <- liftIO $ getActiveEditor ctx
    liftIO $ maybeDo replaceStartOnEditor editor
    where replaceStartOnEditor editor = setEditorReplaceString editor (Just replaceText)

replaceBeforeNext :: Command
replaceBeforeNext = do
    ctx <- S.get
    editor <- liftIO $ getActiveEditor ctx
    liftIO $ maybeDo replaceContinueOnEditor editor
    where replaceContinueOnEditor editor = maybeDo (replaceSelectionWith editor) =<< getEditorReplaceString editor
          replaceSelectionWith editor replaceText = maybeDo (replaceSearchSelectionWith editor replaceText) =<< getEditorSearchString editor
          replaceSearchSelectionWith editor replaceText searchText = do
              buffer <- textViewGetBuffer editor
              (s, e) <- textBufferGetSelectionBounds buffer
              selectedText <- textBufferGetText buffer s e False
              when (searchText == selectedText) $ do
                  textBufferDelete buffer s e
                  textBufferInsert buffer s replaceText



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
