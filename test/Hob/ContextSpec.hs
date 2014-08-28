module Hob.ContextSpec (main, spec) where

import Graphics.UI.Gtk.SourceView (sourceLanguageGetName,
                                   sourceStyleSchemeGetName)
import Hob.Context
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

  describe "sourceLanguage" $
    it "guesses language based on the file ending" $ do
      ctx <- stubbedCtx
      Just lang <- sourceLanguage ctx "/tmp/yyy/test.hs"
      name <- sourceLanguageGetName lang
      name `shouldBe` "Haskell"

  describe "sourceStyleScheme" $
    it "returns source style to use" $ do
      ctx <- stubbedCtx
      Just style <- sourceStyleScheme ctx $ Just "/tmp/yyy/test.hs"
      name <- sourceStyleSchemeGetName style
      name `shouldBe` "Molokai"

stubbedCtx :: IO Context
stubbedCtx = defaultContext "app-data"
