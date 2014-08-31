module Hob.Context.StyleContextSpec (main, spec) where

import Graphics.UI.Gtk.SourceView (sourceStyleSchemeGetName)
import Hob.Context.StyleContext
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "uiFile" $
    it "returns prefixed path" $ do
      ctx <- stubbedCtx
      uiFile ctx `shouldBe` "app-data/ui.glade"

  describe "uiTheme" $
    it "returns prefixed path" $ do
      ctx <- stubbedCtx
      uiTheme ctx `shouldBe` "app-data/themes/gtk/default/gtk-dark.css"

  describe "sourceStyleScheme" $
    it "returns source style to use" $ do
      ctx <- stubbedCtx
      Just style <- sourceStyleScheme ctx $ Just "/tmp/yyy/test.hs"
      name <- sourceStyleSchemeGetName style
      name `shouldBe` "Molokai"

stubbedCtx :: IO StyleContext
stubbedCtx = defaultStyleContext "app-data"
