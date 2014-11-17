module Hob.ContextSpec (main, spec) where

import Control.Monad.Reader
import Data.IORef
import Data.Maybe           (fromJust, isNothing)
import Data.Monoid
import Graphics.UI.Gtk      (Modifier (..))

import Hob.Context

import Test.Hspec

import HobTest.Context.Default


dummyEditor :: Editor
dummyEditor = Editor
            { editorId = \_ -> return 1
            , enterEditorMode = \editor mode -> do
                    currentModes <- runOnEditor modeStack editor
                    return $ editor{modeStack = \_ -> return (currentModes ++ [mode])}
            , exitLastEditorMode = \editor -> do
                    currentModes <- runOnEditor modeStack editor
                    return $ editor{modeStack = \_ -> return (init currentModes)}
            , modeStack      = \_ -> return []
            , isCurrentlyActive  = \_ -> return True
            }


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "command matcher" $ do
        describe "mempty" $ do
            it "does not return any commands on matching a command" $
                expectNoCommandHandler $ matchCommand mempty "/asd"

            it "does not return any commands on matching a key binding" $
                expectNoCommandHandler $ matchKeyBinding mempty ([Control], "S")

        it "combines command matchers on mappend with the identity on the left" $ do
            let matcher = matcherForCommand "test"
            let combinedMatcher = mempty `mappend` matcher
            expectCommandHandler $ matchCommand combinedMatcher "test"

        it "combines key binding matchers on mappend with the identity on the left" $ do
            let matcher = matcherForKeyBinding ([Control], "S")
            let combinedMatcher = mempty `mappend` matcher
            expectCommandHandler $ matchKeyBinding combinedMatcher ([Control], "S")

        it "combines command matchers on mappend with the identity on the right" $ do
            let matcher = matcherForCommand "test"
            let combinedMatcher = matcher `mappend` mempty
            expectCommandHandler $ matchCommand combinedMatcher "test"

        it "combines key binding matchers on mappend with the identity on the right" $ do
            let matcher = matcherForKeyBinding ([Control], "S")
            let combinedMatcher = matcher `mappend` mempty
            expectCommandHandler $ matchKeyBinding combinedMatcher ([Control], "S")

        it "combines command matchers assiciatively" $ do
            let matcher1 = matcherForCommand "test1"
            let matcher2 = matcherForCommand "test2"
            let matcher3 = matcherForCommand "test3"
            let combinedMatcher1 = (matcher1 `mappend` matcher2) `mappend` matcher3
            let combinedMatcher2 = matcher1 `mappend` (matcher2 `mappend` matcher3)
            expectCommandHandler $ matchCommand combinedMatcher1 "test1"
            expectCommandHandler $ matchCommand combinedMatcher1 "test2"
            expectCommandHandler $ matchCommand combinedMatcher1 "test3"
            expectCommandHandler $ matchCommand combinedMatcher2 "test1"
            expectCommandHandler $ matchCommand combinedMatcher2 "test2"
            expectCommandHandler $ matchCommand combinedMatcher2 "test3"
            expectNoCommandHandler $ matchCommand combinedMatcher1 "test"
            expectNoCommandHandler $ matchCommand combinedMatcher2 "test"

        describe "command matcher for prefix" $ do
            it "does not match unknown prefix command" $ do
                let matcher = createMatcherForPrefix "/" $ const emptyHandler
                let matchedHandler = matchCommand matcher "%test text"
                isNothing matchedHandler `shouldBe` True

            it "matches known prefix command" $ do
                handledText <- executeMockedMatcher "/" "/test text"
                handledText `shouldBe` Just "test text"

            it "matches known prefix command with no text" $ do
                handledText <- executeMockedMatcher "/" "/"
                handledText `shouldBe` Just ""

        describe "command matcher for key binding" $ do
            it "does not match unknown key binding" $ do
                let matcher = createMatcherForKeyBinding ([Control], "S") emptyHandler
                let matchedHandler = matchKeyBinding matcher ([Control], "X")
                isNothing matchedHandler `shouldBe` True

            it "matches known key binding" $ do
                ctx <- loadDefaultContext
                (handler, readHandledText) <- recordingHandler
                let matcher = createMatcherForKeyBinding ([Control], "S") $ handler "test"
                let matchedHandler = matchKeyBinding matcher ([Control], "S")
                handledText <- executeRecordingHandler ctx matchedHandler readHandledText
                handledText `shouldBe` Just "test"

        describe "command matcher text command" $ do
            it "does not match unknown text command" $ do
                let matcher = createMatcherForCommand "hi" emptyHandler
                let matchedHandler = matchCommand matcher "ho"
                isNothing matchedHandler `shouldBe` True

            it "matches known command" $ do
                ctx <- loadDefaultContext
                (handler, readHandledText) <- recordingHandler
                let matcher = createMatcherForCommand "hi" $ handler "test"
                let matchedHandler = matchCommand matcher "hi"
                handledText <- executeRecordingHandler ctx matchedHandler readHandledText
                handledText `shouldBe` Just "test"

    describe "app runner" $ do
        it "runs empty monad" $ do
            ctx <- loadDefaultContext
            deferredRunner ctx $ return ()

        it "can defer command execution to IO" $ do
            record <- newIORef (0::Int)
            ctx <- loadDefaultContext
            let countingCommand = liftIO $ modifyIORef record (+1)
            let deferCommand cmd = liftIO $ deferredRunner ctx cmd
            deferCommand (countingCommand >>
                deferCommand (countingCommand >>
                    deferCommand countingCommand))
            invokeCount <- readIORef record
            invokeCount `shouldBe` (3::Int)

    describe "context commands" $ do
        it "returns Nothing for list of modes if there are no editors" $ do
            ctx <- loadContextWithEditors []
            deferredRunner ctx $ enterMode $ Mode "testmode" mempty $ return()
            modes <- getActiveModes ctx
            isNothing modes `shouldBe` True

        it "enters given mode to the active editor" $ do
            ctx <- loadContextWithEditors [dummyEditor]
            deferredRunner ctx $ enterMode $ Mode "testmode" mempty $ return()
            modes <- getActiveModes ctx
            (modeName . last . fromJust) modes `shouldBe` "testmode"

        it "removes mode from the active editor on exitMode" $ do
            ctx <- loadContextWithEditors [dummyEditor]
            deferredRunner ctx $ enterMode $ Mode "testmode" mempty $ return()
            deferredRunner ctx exitLastMode
            modes <- getActiveModes ctx
            (length . fromJust) modes `shouldBe` 0

        it "emits mode change event on entering a mode" $ do
            record <- newIORef False
            ctx <- loadContextWithEditors [dummyEditor]
            deferredRunner ctx $ registerEventHandler (Event "core.mode.change") (liftIO $ writeIORef record True)
            deferredRunner ctx $ enterMode $ Mode "testmode" mempty $ return()
            invoked <- readIORef record
            invoked `shouldBe` True

        it "emits mode change event on exiting a mode" $ do
            record <- newIORef False
            ctx <- loadContextWithEditors [dummyEditor]
            deferredRunner ctx $ enterMode $ Mode "testmode" mempty $ return()
            deferredRunner ctx $ registerEventHandler (Event "core.mode.change") (liftIO $ writeIORef record True)
            deferredRunner ctx exitLastMode
            invoked <- readIORef record
            invoked `shouldBe` True

    describe "event handler support" $ do
        it "invokes a registered handler on fireEvent" $ do
            record <- newIORef False
            ctx <- loadDefaultContext
            deferredRunner ctx $ registerEventHandler (Event "test.event") (liftIO $ writeIORef record True)
            deferredRunner ctx $ emitEvent (Event "test.event")
            invoked <- readIORef record
            invoked `shouldBe` True

        it "does not invoke an event handler with different name" $ do
            record <- newIORef False
            ctx <- loadDefaultContext
            deferredRunner ctx $ registerEventHandler (Event "test.event1") (liftIO $ writeIORef record True)
            deferredRunner ctx $ emitEvent (Event "test.event2")
            invoked <- readIORef record
            invoked `shouldBe` False

loadContextWithEditors :: [Editor] -> IO Context
loadContextWithEditors newEditors = do
    ctx <- loadDefaultContext
    updateEditors (editors ctx) $ const $ return newEditors
    return ctx

getActiveModes :: Context -> IO (Maybe [Mode])
getActiveModes ctx = do
    record <- newIORef Nothing
    deferredRunner ctx $ activeModes >>= (liftIO . writeIORef record . Just)
    Just modes <- readIORef record
    return modes

executeMockedMatcher :: String -> String -> IO (Maybe String)
executeMockedMatcher prefix text = do
    ctx <- loadDefaultContext
    (handler, readHandledText) <- recordingHandler
    let matcher = createMatcherForPrefix prefix handler
    let matchedHandler = matchCommand matcher text
    deferredRunner ctx $ commandExecute $ fromJust matchedHandler
    readHandledText

expectCommandHandler :: Maybe CommandHandler -> Expectation
expectCommandHandler cmdHandler = isNothing cmdHandler `shouldBe` False

expectNoCommandHandler :: Maybe CommandHandler -> Expectation
expectNoCommandHandler cmdHandler = isNothing cmdHandler `shouldBe` True

matcherForCommand :: String -> CommandMatcher
matcherForCommand command = CommandMatcher (const Nothing) $ emptyCommandHandlerForCommand command
    where emptyCommandHandlerForCommand cmd x = if x == cmd then Just emptyHandler else Nothing

matcherForKeyBinding :: KeyboardBinding -> CommandMatcher
matcherForKeyBinding key = CommandMatcher (emptyCommandHandlerForKey key) (const Nothing)
    where emptyCommandHandlerForKey k x = if x == k then Just emptyHandler else Nothing

emptyHandler :: CommandHandler
emptyHandler = CommandHandler Nothing (return())

recordingHandler :: IO (String -> CommandHandler, IO (Maybe String))
recordingHandler = do
    record <- newIORef Nothing
    return (
                \params -> CommandHandler Nothing (liftIO $ writeIORef record $ Just params),
                readIORef record
            )

executeRecordingHandler :: Context -> Maybe CommandHandler -> IO (Maybe String) -> IO (Maybe String)
executeRecordingHandler ctx handler readHandledText = do
    deferredRunner ctx $ commandExecute $ fromJust handler
    readHandledText

