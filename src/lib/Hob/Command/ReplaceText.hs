module Hob.Command.ReplaceText (
        generateReplaceCommandHandler,
        generateReplaceNextCommandHandler,
    ) where

import Control.Monad              ((<=<))
import Data.Text                  (pack)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.SourceView (SourceView)
import System.Glib.GObject        (Quark)

import Hob.Command
import Hob.Command.FindText
import Hob.Context
import Hob.Control
import Hob.Ui.Editor

generateReplaceCommandHandler :: (String -> PreviewCommandHandler) -> (String -> Context -> IO ()) -> String -> CommandHandler
generateReplaceCommandHandler previewCmdHandler executeCmdHandler searchText = 
    CommandHandler (Just $ previewCmdHandler searchText) (executeCmdHandler searchText)

generateReplaceNextCommandHandler :: (Context -> IO ()) -> CommandHandler
generateReplaceNextCommandHandler executeCmdHandler = CommandHandler Nothing executeCmdHandler
