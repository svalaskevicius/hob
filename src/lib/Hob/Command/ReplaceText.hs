module Hob.Command.ReplaceText (
        createMatcherForReplace,
        replaceCommandHandler,
        replaceNextCommandHandler,
        generateReplaceCommandHandler,
        generateReplaceNextCommandHandler,
    ) where

import Control.Monad              ((<=<))
import Data.Text                  (pack)
import Data.Maybe                 (fromJust)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.SourceView (SourceView)
import System.Glib.GObject        (Quark)

import Hob.Command
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

generateReplaceCommandHandler :: (String -> PreviewCommandHandler) -> (String -> Context -> IO ()) -> String -> String -> CommandHandler
generateReplaceCommandHandler previewCmdHandler executeCmdHandler searchText _ = 
    CommandHandler (Just $ previewCmdHandler searchText) (executeCmdHandler searchText)

generateReplaceNextCommandHandler :: (Context -> IO ()) -> CommandHandler
generateReplaceNextCommandHandler executeCmdHandler = CommandHandler Nothing executeCmdHandler

replaceCommandHandler :: String -> String -> CommandHandler
replaceCommandHandler = generateReplaceCommandHandler
                            (\search -> fromJust (commandPreview (searchCommandHandler search)))
                            (\search -> commandExecute (searchCommandHandler search))

replaceNextCommandHandler :: CommandHandler
replaceNextCommandHandler = generateReplaceNextCommandHandler (commandExecute searchNextCommandHandler)
