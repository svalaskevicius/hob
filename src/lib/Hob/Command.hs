module Hob.Command (
    PreviewCommandHandler(..),
    CommandHandler(..),
    CommandMatcher(..),
    KeyboardBinding,
    KeyCommandMatcher,
    TextCommandMatcher,
    createMatcherForPrefix,
    createMatcherForCommand,
    createMatcherForKeyBinding
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
                    commandPreview :: Maybe PreviewCommandHandler,
                    commandExecute :: Context -> IO ()
                }

type KeyboardBinding = ([Modifier], String)
type KeyCommandMatcher = KeyboardBinding -> Maybe CommandHandler
type TextCommandMatcher = String -> Maybe CommandHandler

data CommandMatcher = CommandMatcher {
                    matchKeyBinding :: KeyCommandMatcher,
                    matchCommand    :: TextCommandMatcher
                }

instance Monoid CommandMatcher where
    mempty = CommandMatcher (const Nothing) (const Nothing)
    mappend l r = CommandMatcher (combineMatchKeyBinding l r) (combineMatchCommand l r)

combineMatchKeyBinding :: CommandMatcher -> CommandMatcher -> KeyCommandMatcher
combineMatchKeyBinding = combineMatcher matchKeyBinding

combineMatchCommand :: CommandMatcher -> CommandMatcher -> TextCommandMatcher
combineMatchCommand = combineMatcher matchCommand

combineMatcher :: (CommandMatcher -> a -> Maybe CommandHandler) -> CommandMatcher -> CommandMatcher -> a -> Maybe CommandHandler
combineMatcher combiner l r cmd = if isJust rightResult then rightResult else leftResult
    where leftResult = combiner l cmd
          rightResult = combiner r cmd

createMatcherForPrefix :: String -> (String -> CommandHandler) -> CommandMatcher
createMatcherForPrefix prefix handler = CommandMatcher (const Nothing) (matchHandler prefix)
    where
        matchHandler :: String -> String -> Maybe CommandHandler
        matchHandler (p:xs) (t:xt) = if p == t then matchHandler xs xt else Nothing
        matchHandler "" params = Just $ handler params
        matchHandler _ "" = Nothing

createMatcherForCommand :: String -> CommandHandler -> CommandMatcher
createMatcherForCommand command handler = CommandMatcher (const Nothing) (matchHandler command)
    where
        matchHandler :: String -> String -> Maybe CommandHandler
        matchHandler (p:xs) (t:xt) = if p == t then matchHandler xs xt else Nothing
        matchHandler "" "" = Just handler
        matchHandler _ "" = Nothing
        matchHandler "" _ = Nothing

createMatcherForKeyBinding :: KeyboardBinding -> CommandHandler -> CommandMatcher
createMatcherForKeyBinding keyBinding handler = CommandMatcher (matchHandler keyBinding) (const Nothing)
    where
        matchHandler :: KeyboardBinding -> KeyboardBinding -> Maybe CommandHandler
        matchHandler boundKey matchedKey = if boundKey == matchedKey then Just handler else Nothing

