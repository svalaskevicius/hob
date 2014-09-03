module Hob.Command (
    PreviewCommand(..),
    Command(..),
    CommandMatcher(..)
) where

import Graphics.UI.Gtk (Modifier)

import Hob.Context

data PreviewCommand = PreviewCommand {
                    previewExecute :: Context -> IO (),
                    previewReset :: Context -> IO ()
                }

data Command = Command {
                    commandPreview :: Maybe (PreviewCommand),
                    commandExecute :: Context -> IO ()
                }

data CommandMatcher = CommandMatcher {
                    matchKeyBinding :: ([Modifier], String) -> Maybe (Command),
                    matchCommand :: String -> Maybe (Command)
                }
