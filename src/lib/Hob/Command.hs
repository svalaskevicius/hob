module Hob.Command (
    PreviewCommandHandler(..),
    CommandHandler(..),
    CommandMatcher(..),
    KeyboardBinding(..),
    mempty, mappend
) where

import Graphics.UI.Gtk (Modifier)
import Data.Monoid
import Data.Maybe (isJust)
import Hob.Context

data PreviewCommandHandler = PreviewCommandHandler {
                    previewExecute :: Context -> IO (),
                    previewReset :: Context -> IO ()
                }

data CommandHandler = CommandHandler {
                    commandPreview :: Maybe (PreviewCommandHandler),
                    commandExecute :: Context -> IO ()
                }
                
type KeyboardBinding = ([Modifier], String)

data CommandMatcher = CommandMatcher {
                    matchKeyBinding :: KeyboardBinding -> Maybe (CommandHandler),
                    matchCommand :: String -> Maybe (CommandHandler)
                }

instance Monoid CommandMatcher where
    mempty = CommandMatcher (\_->Nothing) (\_->Nothing)
    mappend l r = CommandMatcher (combineMatchKeyBinding l r) (combineMatchCommand l r)

combineMatchKeyBinding :: CommandMatcher -> CommandMatcher -> KeyboardBinding -> Maybe (CommandHandler)
combineMatchKeyBinding l r cmd = if isJust rightResult then rightResult else leftResult
    where leftResult = matchKeyBinding l cmd
          rightResult = matchKeyBinding r cmd
          
combineMatchCommand :: CommandMatcher -> CommandMatcher -> String -> Maybe (CommandHandler)
combineMatchCommand l r cmd = if isJust rightResult then rightResult else leftResult
    where leftResult = matchCommand l cmd
          rightResult = matchCommand r cmd
          
