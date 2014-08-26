module Hob.ContextSpec (main, spec) where

import Hob.Context
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "uiFile" $ do
    it "returns absolute path" $ do
      let ctx = Context "/tmp/xxx"
      (uiFile ctx) `shouldBe` "/tmp/xxx/ui/ui.glade"
