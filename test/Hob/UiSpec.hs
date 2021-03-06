module Hob.UiSpec (main, spec) where

import Graphics.UI.Gtk

import qualified Hob.Context           as HC
import qualified Hob.Context.UiContext as HC

import Test.Hspec

import HobTest.Context.Default

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "mainWindow" $
    it "is named" $ do
      ctx <- loadDefaultContext
      name <- widgetGetName $ HC.mainWindow . HC.uiContext $ ctx
      name `shouldBe` "mainWindow"
