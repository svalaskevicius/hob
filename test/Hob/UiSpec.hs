module Hob.UiSpec where

import Test.Hspec

import Graphics.UI.Gtk
import Hob.Ui

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
  describe "creates mainWindow" $ do
    it "returns GtkWindow" $ do
      mainWindow <- loadGui
      name <- widgetGetName mainWindow
      name `shouldBe` "GtkWindow"
