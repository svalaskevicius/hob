{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

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
import Data.Maybe               (isJust)
import Data.Monoid
import Graphics.UI.Gtk          (Modifier)
import GtkExtras.LargeTreeStore as LTS (TreeStore)

import Hob.Context.FileContext
import Hob.Context.StyleContext
import Hob.Context.UiContext
import Hob.DirectoryTree


type App = ReaderT Context IO

newtype Event = Event String deriving (Eq)

data Editor = Editor {
    editorId           :: Editor -> App Int,
    enterEditorMode    :: Editor -> Mode -> App Editor,
    exitLastEditorMode :: Editor -> App Editor,
    modeStack          :: Editor -> App [Mode],
    isCurrentlyActive  :: Editor -> App Bool
}

runOnEditor :: (Editor -> Editor -> a) -> Editor -> a
runOnEditor f editor = f editor editor

data EventBus = EventBus {
    addListener       :: Event -> App() -> IO(),
    listenersForEvent :: Event -> IO [App()]
}

data EditorList = EditorList {
    updateEditors :: ([Editor] -> IO [Editor]) -> IO(),
    getEditors    :: IO [Editor]
}

data Context = Context {
    styleContext  :: StyleContext,
    fileContext   :: FileContext,
    uiContext     :: UiContext,
    fileTreeStore :: LTS.TreeStore DirectoryTreeElement,
    baseCommands  :: CommandMatcher,
    editors       :: EditorList,
    messageLoop   :: Context -> Message -> IO(),
    eventBus      :: EventBus,
    idGenerator   :: IO Int
}

data Mode = Mode {
    modeName       :: String,
    commandMatcher :: CommandMatcher,
    cleanup        :: App()
}

data Message = AppAction (App())

runApp :: Context -> App a -> IO a
runApp ctx appSteps = runReaderT appSteps ctx

runMessage :: Context -> Message -> IO Context
runMessage  ctx (AppAction action) = runApp ctx action >> return ctx

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

initEventBus :: IO EventBus
initEventBus = do
    bus <- newMVar []
    return $ EventBus (addEventListener bus) (lookupEvent bus)
    where
        addEventListener bus event action = do
            listeners <- takeMVar bus
            putMVar bus $ combineEventListeners event action listeners

        combineEventListeners :: Event -> App() -> [(Event, [App()])] -> [(Event, [App()])]
        combineEventListeners event action [] = [(event, [action])]
        combineEventListeners event action (x@(evt, initHandlers):xs) =
            if evt == event then (evt, action:initHandlers) : xs
            else x : combineEventListeners event action xs

        lookupEvent bus event = do
            listeners <- readMVar bus
            return $ findEvent event listeners

        findEvent _ [] = []
        findEvent event ((evt, handlers):xs) = if evt == event then handlers
                                               else findEvent event xs

initEditors :: IO EditorList
initEditors = do
    editorList <- newMVar []
    return $ EditorList (updateEditorsHandler editorList) (getEditorsHandler editorList)
    where
        updateEditorsHandler editorList updater = do
            oldEditors <- takeMVar editorList
            newEditors <- updater oldEditors
            putMVar editorList newEditors
        getEditorsHandler = readMVar

deferredRunner :: Context -> App() -> IO()
deferredRunner ctx actions = messageLoop ctx ctx $ AppAction actions

registerEventHandler :: Event -> App() -> App()
registerEventHandler event handler = do
    bus <- fromContext eventBus
    liftIO $ addListener bus event handler

emitEvent :: Event -> App()
emitEvent event = do
    bus <- fromContext eventBus
    handlers <- liftIO $ listenersForEvent bus event
    sequence_ handlers

fromContext :: forall r a (m :: * -> *). MonadReader r m => (r -> a) -> m a
fromContext = asks

currentEditor :: App (Maybe Editor)
currentEditor = do
    editorList <- fromContext editors
    active <- filterM (\e -> isCurrentlyActive e e) =<< liftIO (getEditors editorList)
    return $
        if null active then Nothing
        else Just $ head active

enterMode :: Mode -> App()
enterMode mode = do
    updateActiveEditor $ \editor -> runOnEditor enterEditorMode editor mode
    emitEvent $ Event "core.mode.change"

activeModes :: App (Maybe [Mode])
activeModes = do
    active <- currentEditor
    maybe (return Nothing) (runOnEditor modeStack >=> return . Just) active

exitLastMode :: App()
exitLastMode = do
    updateActiveEditor $ \editor -> runOnEditor exitLastEditorMode editor
    emitEvent $ Event "core.mode.change"

updateActiveEditor :: (Editor -> App Editor) -> App()
updateActiveEditor actions = do
    ctx <- ask
    editorList <- fromContext editors
    liftIO $ updateEditors editorList $ \oldEditors -> runReaderT (updateActiveEditorHandler oldEditors) ctx
    where
        updateActiveEditorHandler oldEditors = do
            (e1, e2) <- splitBeforeFirstActive oldEditors
            if null e2 then return oldEditors
            else do
                let active = head e2
                active' <- actions active
                return $ e1 ++ [active'] ++ tail e2
        splitBeforeFirstActive [] = return ([], [])
        splitBeforeFirstActive (x:xs) = do
            active <- runOnEditor isCurrentlyActive x
            if active then return ([], x:xs)
            else do
                (n, ns) <- splitBeforeFirstActive xs
                return (x:n, ns)


getActiveCommands :: App CommandMatcher
getActiveCommands = do
    base <- asks baseCommands
    maybeModes <- activeModes
    maybe (return base)
          (\modes -> return $ base `mappend` mconcat (fmap commandMatcher modes))
          maybeModes

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

initIdGenerator :: IO (IO Int)
initIdGenerator = do
    generatorStorage <- newMVar 0
    return $ do
        lastGeneratdId <- takeMVar generatorStorage
        let newId = lastGeneratdId + 1
        putMVar generatorStorage newId
        return newId

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


