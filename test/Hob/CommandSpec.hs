module Hob.CommandSpec (main, spec) where

import Data.Maybe      (isNothing)
import Data.Monoid
import Graphics.UI.Gtk (Modifier (..))
import Hob.Command
import Test.Hspec


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

expectCommandHandler :: Maybe CommandHandler -> Expectation
expectCommandHandler cmdHandler = isNothing cmdHandler `shouldBe` False

expectNoCommandHandler :: Maybe CommandHandler -> Expectation
expectNoCommandHandler cmdHandler = isNothing cmdHandler `shouldBe` True

matcherForCommand :: String -> CommandMatcher
matcherForCommand command = CommandMatcher (const Nothing) $ emptyCommandHandlerForCommand command
    where emptyCommandHandlerForCommand cmd x = if x == cmd then emptyHandler else Nothing
          emptyHandler = Just $ CommandHandler Nothing (\ _ -> return())

matcherForKeyBinding :: KeyboardBinding -> CommandMatcher
matcherForKeyBinding key = CommandMatcher (emptyCommandHandlerForKey key) (const Nothing)
    where emptyCommandHandlerForKey k x = if x == k then emptyHandler else Nothing
          emptyHandler = Just $ CommandHandler Nothing (\ _ -> return())
