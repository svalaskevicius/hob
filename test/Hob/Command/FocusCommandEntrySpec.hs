module Hob.Command.FocusCommandEntrySpec (main, spec) where

import Data.Maybe
import Graphics.UI.Gtk

import           Hob.Command
import           Hob.Command.FocusCommandEntry
import           Hob.Command.NewTab
import qualified Hob.Context                   as HC
import qualified Hob.Context.UiContext         as HC
import           Hob.Ui

import Test.Hspec

import HobTest.Context.Default

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
      commandExecute toggleFocusOnCommandEntryCommandHandler ctx
      focused <- toggleFocusOnCommandEntryAndReturnState ctx
      focused `shouldBe` True

    it "focus moves to editor on toggle" $ do
      ctx <- launchNewFile
      commandExecute toggleFocusOnCommandEntryCommandHandler ctx
      commandFocused <- toggleFocusOnCommandEntryAndReturnState ctx
      editorFocused <- widgetGetIsFocus . fromJust =<< getActiveEditor ctx
      commandFocused `shouldBe` False
      editorFocused `shouldBe` True

launchNewFile :: IO HC.Context
launchNewFile = do
    ctx <- loadDefaultContext
    editNewFile ctx
    return ctx

toggleFocusOnCommandEntryAndReturnState :: HC.Context -> IO Bool
toggleFocusOnCommandEntryAndReturnState ctx = do
    commandExecute toggleFocusOnCommandEntryCommandHandler ctx
    widgetGetIsFocus $ HC.commandEntry . HC.uiContext $ ctx
