module Hob.Context.FileContextSpec (main, spec) where

import Data.Text                  (pack)
import Graphics.UI.Gtk.SourceView (sourceLanguageGetName)
import Hob.Context.FileContext
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

stubbedCtx :: IO FileContext
stubbedCtx = defaultFileContext emptyFileLoader blackholeFileWriter emptyFileTree

blackholeFileWriter :: FileWriter
blackholeFileWriter _ _ = return ()

emptyFileTree :: FileTreeLoader
emptyFileTree = return []

emptyFileLoader :: FileLoader
emptyFileLoader _ = return $ Just $ pack ""
