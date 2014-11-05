module Hob.Context (
    App,
    Context(..),
    PreviewCommandHandler(..),
    CommandHandler(..),
    CommandMatcher(..),
    KeyboardBinding,
    KeyCommandMatcher,
    TextCommandMatcher,
    Mode(..),
    Event(..),
    initContext,
    enterMode,
    exitLastMode,
    activeModes,
    deferredRunner,
    registerEventHandler,
    emitEvent,
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
newtype Event = Event String deriving (Eq)

data Context = Context {
    styleContext   :: StyleContext,
    fileContext    :: FileContext,
    uiContext      :: UiContext,
    fileTreeStore  :: LTS.TreeStore DirectoryTreeElement,
    modeStack      :: [Mode],
    messageLoop    :: Message -> IO(),
    currentContext :: IO Context,
    eventListeners :: [(Event, [App()])]
}

data Mode = Mode {
    modeName       :: String,
    commandMatcher :: CommandMatcher,
    cleanup        :: App()
}

data Message = AppAction (App())

runApp :: Context -> App () -> IO Context
runApp ctx appSteps =  do
    ret <- runStateT appSteps ctx
    return $ snd ret

runMessage :: Context -> Message -> IO Context
runMessage  ctx (AppAction action) = runApp ctx action

initContext :: StyleContext -> FileContext -> UiContext -> LTS.TreeStore DirectoryTreeElement -> Mode -> IO Context
initContext styleCtx fileCtx uiCtx treeModel initMode = do
    ctxRef <- newEmptyMVar
    deferredMessagesRef <- newMVar []
    let ctx = Context styleCtx fileCtx uiCtx treeModel [initMode] (messageRunner ctxRef deferredMessagesRef) (readMVar ctxRef) []
    putMVar ctxRef ctx
    return ctx
    where
        messageRunner ctxRef deferredMessagesRef message = do
            queueMessage deferredMessagesRef message
            mCtx <- tryTakeMVar ctxRef
            case mCtx of
                Just ctx -> flushMessageQueue deferredMessagesRef ctx >>= putMVar ctxRef
                Nothing -> return()
        flushMessageQueue deferredMessagesRef ctx = do
            messages <- swapMVar deferredMessagesRef []
            if null messages then return ctx
            else foldM runMessage ctx messages >>= flushMessageQueue deferredMessagesRef        
        queueMessage deferredMessagesRef message = modifyMVar_ deferredMessagesRef (\messages -> return $ messages ++ [message])


deferredRunner :: Context -> App() -> IO()
deferredRunner ctx actions = messageLoop ctx $ AppAction actions

registerEventHandler :: Event -> App() -> App()
registerEventHandler event handler = do
    ctx <- get
    put ctx{eventListeners = addEventHandler $ eventListeners ctx}
    where addEventHandler :: [(Event, [App()])] -> [(Event, [App()])]
          addEventHandler [] = [(event, [handler])]
          addEventHandler (x@(evt, initHandlers):xs) = 
                if evt == event then (evt, handler:initHandlers) : xs
                else x : addEventHandler xs
                                    
emitEvent :: Event -> App()
emitEvent event = do
    ctx <- get
    let handlers = findEvent $ eventListeners ctx
    sequence_ $ handlers
    where findEvent [] = []
          findEvent ((evt, handlers):xs) = if evt == event then handlers
                                           else findEvent xs


enterMode :: Mode -> App()
enterMode mode = do
    ctx <- get
    put ctx{modeStack = modeStack ctx++[mode]}

activeModes :: App [Mode]
activeModes = do
    ctx <- get
    return $ modeStack ctx

exitLastMode :: App()
exitLastMode = do
    ctx <- get
    let modes = modeStack ctx
    if length modes > 1 then do
        let lastMode = last modes
        put ctx{modeStack = init modes}
        cleanup lastMode
    else return()


data PreviewCommandHandler = PreviewCommandHandler {
                    previewExecute :: App(),
                    previewReset   :: App()
                }

data CommandHandler = CommandHandler {
                    commandPreview :: Maybe PreviewCommandHandler,
                    commandExecute :: App()
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


