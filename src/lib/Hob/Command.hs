module Hob.Command (
    PreviewCommandHandler(..),
    CommandHandler(..),
    CommandMatcher(..),
    KeyboardBinding(..),
    mempty, mappend
) where

import Data.Maybe      (isJust)
import Data.Monoid
import Graphics.UI.Gtk (Modifier)
import Hob.Context

data PreviewCommandHandler = PreviewCommandHandler {
                    previewExecute :: Context -> IO (),
                    previewReset   :: Context -> IO ()
                }

data CommandHandler = CommandHandler {
                    commandPreview :: Maybe (PreviewCommandHandler),
                    commandExecute :: Context -> IO ()
                }

type KeyboardBinding = ([Modifier], String)

data CommandMatcher = CommandMatcher {
                    matchKeyBinding :: KeyboardBinding -> Maybe (CommandHandler),
                    matchCommand    :: String -> Maybe (CommandHandler)
                }

instance Monoid CommandMatcher where
    mempty = CommandMatcher (\_->Nothing) (\_->Nothing)
    mappend l r = CommandMatcher (combineMatchKeyBinding l r) (combineMatchCommand l r)

combineMatchKeyBinding :: CommandMatcher -> CommandMatcher -> KeyboardBinding -> Maybe (CommandHandler)
combineMatchKeyBinding = combineMatcher matchKeyBinding

combineMatchCommand :: CommandMatcher -> CommandMatcher -> String -> Maybe (CommandHandler)
combineMatchCommand = combineMatcher matchCommand

combineMatcher :: (CommandMatcher -> a -> Maybe (CommandHandler)) -> CommandMatcher -> CommandMatcher -> a -> Maybe (CommandHandler)
combineMatcher combiner l r cmd = if isJust rightResult then rightResult else leftResult
    where leftResult = combiner l cmd
          rightResult = combiner r cmd
