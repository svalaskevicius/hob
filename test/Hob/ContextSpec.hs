module Hob.ContextSpec (main, spec) where

import Graphics.UI.Gtk.SourceView (sourceLanguageGetName)
import Hob.Context
import Hob.Context.StyleContext
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "sourceLanguage" $
    it "guesses language based on the file ending" $ do
      ctx <- stubbedCtx
      Just lang <- sourceLanguage ctx "/tmp/yyy/test.hs"
      name <- sourceLanguageGetName lang
      name `shouldBe` "Haskell"

stubbedCtx :: IO Context
stubbedCtx = defaultContext =<< defaultStyleContext "app-data"
