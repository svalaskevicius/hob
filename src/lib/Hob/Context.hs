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
    Editor(..),
    EditorList(..),
    initContext,
    runOnEditor,
    enterMode,
    exitLastMode,
    activeModes,
    deferredRunner,
    registerEventHandler,
    emitEvent,
    createMatcherForPrefix,
    createMatcherForCommand,
    createMatcherForKeyBinding,
    fromContext,
    runApp,
    getActiveCommands,
) where

import Control.Concurrent.MVar
import Control.Monad.Reader
import GtkExtras.LargeTreeStore as LTS (TreeStore)

import Hob.Context.CommandMatcher
import Hob.Context.Editor
import Hob.Context.Events
import Hob.Context.FileContext
import Hob.Context.StyleContext
import Hob.Context.Types
import Hob.Context.UiContext
import Hob.DirectoryTree



initContext :: StyleContext -> FileContext -> UiContext -> LTS.TreeStore DirectoryTreeElement -> CommandMatcher -> IO Context
initContext styleCtx fileCtx uiCtx treeModel initCommands = do
    actionSync <- newMVar True
    deferredMessagesRef <- newMVar []
    bus <- initEventBus
    editorList <- initEditors
    initialisedIdGenerator <- initIdGenerator
    let ctx = Context styleCtx fileCtx uiCtx treeModel initCommands editorList (messageRunner actionSync deferredMessagesRef) bus initialisedIdGenerator
    return ctx
    where
        messageRunner actionSync deferredMessagesRef ctx message = do
            queueMessage deferredMessagesRef message
            sync <- tryTakeMVar actionSync
            case sync of
                Just _ -> flushMessageQueue deferredMessagesRef ctx >> putMVar actionSync True
                Nothing -> return()
        flushMessageQueue deferredMessagesRef ctx = do
            messages <- swapMVar deferredMessagesRef []
            unless (null messages) $ foldM runMessage ctx messages >>= flushMessageQueue deferredMessagesRef
        queueMessage deferredMessagesRef message = modifyMVar_ deferredMessagesRef (\messages -> return $ messages ++ [message])

runApp :: Context -> App a -> IO a
runApp ctx appSteps = runReaderT appSteps ctx

runMessage :: Context -> Message -> IO Context
runMessage  ctx (AppAction action) = runApp ctx action >> return ctx

deferredRunner :: Context -> App() -> IO()
deferredRunner ctx actions = messageLoop ctx ctx $ AppAction actions

initIdGenerator :: IO (IO Int)
initIdGenerator = do
    generatorStorage <- newMVar 0
    return $ do
        lastGeneratdId <- takeMVar generatorStorage
        let newId = lastGeneratdId + 1
        putMVar generatorStorage newId
        return newId


