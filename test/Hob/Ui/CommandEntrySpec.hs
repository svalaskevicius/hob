module Hob.Ui.CommandEntrySpec (main, spec) where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.General.StyleContext

import qualified Hob.Context as HC

import Test.Hspec

import HobTest.Context.Default

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "command entry" $ do
    it "is named" $ do
      ctx <- loadDefaultContext
      name <- widgetGetName $ HC.commandEntry ctx
      name `shouldBe` "commandEntry"

    it "initially there is no error class applied" $ do
      ctx <- loadDefaultContext
      styleContext <- widgetGetStyleContext $ HC.commandEntry ctx
      hasErrorClass <- styleContextHasClass styleContext "error"
      hasErrorClass `shouldBe` False

    it "applies error style class if the command is unknown" $ do
      ctx <- loadDefaultContext
      let commandEntry = HC.commandEntry ctx
      entrySetText commandEntry "qweqwe"
      styleContext <- widgetGetStyleContext commandEntry
      hasErrorClass <- styleContextHasClass styleContext "error"
      hasErrorClass `shouldBe` True

    it "removes error style on empty command" $ do
      (_, commandEntry, styleContext) <- loadDefaultGuiWithCommandAndItsStyleContext
      entrySetText commandEntry "not empty"
      styleContextAddClass styleContext "error"
      entrySetText commandEntry ""
      hasErrorClass <- styleContextHasClass styleContext "error"
      hasErrorClass `shouldBe` False

    it "removes error style on search command" $ do
      (_, commandEntry, styleContext) <- loadDefaultGuiWithCommandAndItsStyleContext
      styleContextAddClass styleContext "error"
      entrySetText commandEntry "/asd"
      hasErrorClass <- styleContextHasClass styleContext "error"
      hasErrorClass `shouldBe` False

loadDefaultGuiWithCommandAndItsStyleContext :: IO (HC.Context, Entry, StyleContext)
loadDefaultGuiWithCommandAndItsStyleContext = do
    ctx <- loadDefaultContext
    let commandEntry = HC.commandEntry ctx
    styleContext <- widgetGetStyleContext commandEntry
    return (ctx, commandEntry, styleContext)
