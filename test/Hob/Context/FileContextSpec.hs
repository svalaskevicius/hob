module Hob.Context.FileContextSpec (main, spec) where

import Graphics.UI.Gtk.SourceView (sourceLanguageGetName)
import Hob.Context.FileContext
import Test.Hspec

import HobTest.Context.Default

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "sourceLanguage" $
    it "guesses language based on the file ending" $ do
      ctx <- stubbedCtx
      Just lang <- sourceLanguage ctx "/tmp/yyy/test.hs"
      name <- sourceLanguageGetName lang
      name `shouldBe` "Haskell"

stubbedCtx :: IO FileContext
stubbedCtx = defaultFileContext emptyFileLoader blackholeFileWriter emptyFileTree

