module Hob.Context (
    App,
    Context(..),
    Command,
    PreviewCommandHandler(..),
    CommandHandler(..),
    CommandMatcher(..),
    KeyboardBinding,
    KeyCommandMatcher,
    TextCommandMatcher,
    Mode(..),
    initContext,
    createMatcherForPrefix,
    createMatcherForCommand,
    createMatcherForKeyBinding,
) where

import Control.Concurrent.MVar
import Control.Monad.State
import Data.Maybe               (isJust)
import Data.Monoid
import Graphics.UI.Gtk          (Modifier)
import GtkExtras.LargeTreeStore as LTS (TreeStore)

import Hob.Context.FileContext
import Hob.Context.StyleContext
import Hob.Context.UiContext
import Hob.DirectoryTree


type App = StateT Context IO

data Context = Context {
    styleContext   :: StyleContext,
    fileContext    :: FileContext,
    uiContext      :: UiContext,
    fileTreeStore  :: LTS.TreeStore DirectoryTreeElement,
    modeStack      :: [Mode],
    deferredRunner :: Command -> IO()
}

type Command = App ()

runApp :: Context -> App () -> IO Context
runApp ctx appSteps =  do
    ret <- runStateT appSteps ctx
    return $ snd ret

initContext :: StyleContext -> FileContext -> UiContext -> LTS.TreeStore DirectoryTreeElement -> Mode -> IO Context
initContext styleCtx fileCtx uiCtx treeModel initMode = do
    ctxRef <- newEmptyMVar
    deferredCommandsRef <- newMVar []
    let ctx = Context styleCtx fileCtx uiCtx treeModel [initMode] (commandRunner ctxRef deferredCommandsRef)
    putMVar ctxRef ctx
    return ctx
    where
        commandRunner ctxRef deferredCommandsRef command = do
            queueCommand deferredCommandsRef command
            mCtx <- tryTakeMVar ctxRef
            case mCtx of
                Just ctx -> putMVar ctxRef =<< flushCommandQueue deferredCommandsRef ctx
                Nothing -> return()
        flushCommandQueue deferredCommandsRef ctx = do
            commands <- swapMVar deferredCommandsRef []
            if null commands then return ctx
            else foldM runApp ctx commands >>= flushCommandQueue deferredCommandsRef
        queueCommand deferredCommandsRef command = modifyMVar_ deferredCommandsRef (\cmds -> return $ cmds ++ [command])

data PreviewCommandHandler = PreviewCommandHandler {
                    previewExecute :: Command,
                    previewReset   :: Command
                }

data CommandHandler = CommandHandler {
                    commandPreview :: Maybe PreviewCommandHandler,
                    commandExecute :: Command
                }

type KeyboardBinding = ([Modifier], String)
type SingleCommandMatcher a = a -> Maybe CommandHandler
type KeyCommandMatcher = SingleCommandMatcher KeyboardBinding
type TextCommandMatcher = SingleCommandMatcher String

data CommandMatcher = CommandMatcher {
                    matchKeyBinding :: KeyCommandMatcher,
                    matchCommand    :: TextCommandMatcher
                }

instance Monoid CommandMatcher where
    mempty = CommandMatcher (const Nothing) (const Nothing)
    mappend l r = CommandMatcher (combineMatchKeyBinding l r) (combineMatchCommand l r)
        where combineMatchKeyBinding = combineMatcher matchKeyBinding
              combineMatchCommand = combineMatcher matchCommand

combineMatcher :: (CommandMatcher -> SingleCommandMatcher a) -> CommandMatcher -> CommandMatcher -> SingleCommandMatcher a
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


data Mode = Mode {
    modeName       :: String,
    commandMatcher :: CommandMatcher
}
