module Hob.Command.FocusCommandEntrySpec (main, spec) where

import Data.Maybe
import Graphics.UI.Gtk

import           Hob.Command.FocusCommandEntry
import           Hob.Command.NewTab
import           Hob.Context
import qualified Hob.Context.UiContext         as HC
import           Hob.Ui

import Test.Hspec

import HobTest.Context.Default
import HobTest.Control

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "focus command entry command" $ do
    it "focuses the command entry" $ do
      ctx <- loadDefaultContext
      focused <- toggleFocusOnCommandEntryAndReturnState ctx
      focused `shouldBe` True

    it "focus stays on toggle if there is no editor to focus to" $ do
      ctx <- loadDefaultContext
      runCtxActions ctx $ commandExecute toggleFocusOnCommandEntryCommandHandler
      focused <- toggleFocusOnCommandEntryAndReturnState ctx
      focused `shouldBe` True

    it "focus moves to editor on toggle" $ do
      ctx <- launchNewFile
      runCtxActions ctx $ commandExecute toggleFocusOnCommandEntryCommandHandler
      commandFocused <- toggleFocusOnCommandEntryAndReturnState ctx
      editorFocused <- widgetGetIsFocus . fromJust =<< getActiveEditor ctx
      commandFocused `shouldBe` False
      editorFocused `shouldBe` True

launchNewFile :: IO Context
launchNewFile = do
    ctx <- loadDefaultContext
    runCtxActions ctx editNewFile
    return ctx

toggleFocusOnCommandEntryAndReturnState :: Context -> IO Bool
toggleFocusOnCommandEntryAndReturnState ctx = do
    runCtxActions ctx $ commandExecute toggleFocusOnCommandEntryCommandHandler
    widgetGetIsFocus $ HC.commandEntry . uiContext $ ctx
