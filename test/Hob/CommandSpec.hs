module Hob.CommandSpec (main, spec) where

import Test.Hspec
import Data.Maybe (isNothing)
import Graphics.UI.Gtk (Modifier(..))
import Hob.Command


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "command matcher" $ do
        describe "mempty" $ do
            it "does not return any commands on matching a command" $ do
                let cmd = matchCommand mempty "/asd"
                isNothing cmd `shouldBe` True
                
            it "does not return any commands on matching a key binding" $ do
                let cmd = matchKeyBinding mempty ([Control], "S")
                isNothing cmd `shouldBe` True

        it "combines command matchers on mappend with the identity on the left" $ do
            let testCommand "test" = Just $ CommandHandler Nothing (\_ -> return())
                testCommand _ = Nothing
            let matcher = CommandMatcher (\_->Nothing) testCommand
            let combinedMatcher = mempty `mappend` matcher
            let cmd = matchCommand combinedMatcher "test"
            isNothing cmd `shouldBe` False

        it "combines key binding matchers on mappend with the identity on the left" $ do
            let testKeyCommand ([Control], "S") = Just $ CommandHandler Nothing (\_ -> return())
                testKeyCommand _ = Nothing
            let matcher = CommandMatcher testKeyCommand (\_->Nothing) 
            let combinedMatcher = mempty `mappend` matcher
            let cmd = matchKeyBinding combinedMatcher ([Control], "S")
            isNothing cmd `shouldBe` False

        it "combines command matchers on mappend with the identity on the right" $ do
            let testCommand "test" = Just $ CommandHandler Nothing (\_ -> return())
                testCommand _ = Nothing
            let matcher = CommandMatcher (\_->Nothing) testCommand
            let combinedMatcher = matcher `mappend` mempty
            let cmd = matchCommand combinedMatcher "test"
            isNothing cmd `shouldBe` False

        it "combines key binding matchers on mappend with the identity on the right" $ do
            let testKeyCommand ([Control], "S") = Just $ CommandHandler Nothing (\_ -> return())
                testKeyCommand _ = Nothing
            let matcher = CommandMatcher testKeyCommand (\_->Nothing) 
            let combinedMatcher = matcher `mappend` mempty
            let cmd = matchKeyBinding combinedMatcher ([Control], "S")
            isNothing cmd `shouldBe` False
            