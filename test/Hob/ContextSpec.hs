module Hob.ContextSpec (main, spec) where

import Hob.Context
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "uiFile" $
    it "returns prefixed path" $
      uiFile (Context "/tmp/xxx") `shouldBe` "/tmp/xxx/ui/ui.glade"

  describe "uiTheme" $
    it "returns prefixed path" $
      uiTheme (Context "/tmp/xxx") `shouldBe` "/tmp/xxx/ui/themes/gtk/default/gtk-dark.css"

