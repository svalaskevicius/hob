module Hob.UiSpec where

import Test.Hspec
import Data.Maybe
import Graphics.UI.Gtk
import Hob.Ui

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
  describe "creates mainWindow" $ do
    it "returns named main window" $ do
      mainWindow <- loadGui
      name <- widgetGetName mainWindow
      name `shouldBe` "mainWindow"
    it "contains sidebar" $ do
      mainWindow <- loadGui
      paned <- binGetChild mainWindow
      scrollbar <- panedGetChild1 $ castToPaned $ fromJust paned
      sidebar <- binGetChild $ castToScrolledWindow $ fromJust scrollbar
      name <- widgetGetName $ fromJust sidebar
      name `shouldBe` "directoryListing"
