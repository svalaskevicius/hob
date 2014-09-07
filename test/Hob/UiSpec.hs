module Hob.UiSpec (main, spec) where

import Graphics.UI.Gtk

import qualified Hob.Context as HC

import Test.Hspec

import HobTest.Context.Default

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "mainWindow" $
    it "is named" $ do
      ctx <- loadDefaultContext
      name <- widgetGetName $ HC.mainWindow ctx
      name `shouldBe` "mainWindow"
