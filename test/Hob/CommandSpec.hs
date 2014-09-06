module Hob.CommandSpec (main, spec) where

import Data.Maybe      (fromJust, isNothing)
import Data.Monoid
import Graphics.UI.Gtk (Modifier (..))
import Hob.Command
import Test.Hspec

import Data.IORef
import Data.Text  (pack)
import Hob.Ui     (loadGui)

import qualified Hob.Context              as HC
import qualified Hob.Context.FileContext  as HFC
import qualified Hob.Context.StyleContext as HSC

main :: IO ()
main = hspec spec

spec :: Spec
spec =
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

executeMockedMatcher :: String -> String -> IO (Maybe String)
executeMockedMatcher prefix text = do
    ctx <- loadDefaultGui
    (handler, readHandledText) <- recordingHandler
    let matcher = createMatcherForPrefix prefix handler
    let matchedHandler = matchCommand matcher text
    commandExecute (fromJust matchedHandler) ctx
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
emptyHandler = CommandHandler Nothing (\ _ -> return())

recordingHandler :: IO (String -> CommandHandler, IO (Maybe String))
recordingHandler = do
    state <- newIORef Nothing
    return (
                \params -> CommandHandler Nothing (\_ -> writeIORef state $ Just params),
                readIORef state
            )

blackholeFileWriter :: HFC.FileWriter
blackholeFileWriter _ _ = return ()

emptyFileTree :: HFC.FileTreeLoader
emptyFileTree = return []

emptyFileLoader :: HFC.FileLoader
emptyFileLoader _ = return $ Just $ pack ""

loadDefaultGui :: IO HC.Context
loadDefaultGui = do
    sc <- HSC.defaultStyleContext "app-data"
    fc <- HFC.defaultFileContext emptyFileLoader blackholeFileWriter emptyFileTree
    loadGui fc sc
