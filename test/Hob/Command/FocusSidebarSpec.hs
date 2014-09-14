module Hob.Command.FocusSidebarSpec (main, spec) where

import Graphics.UI.Gtk

import           Hob.Command
import           Hob.Command.FocusSidebar
import qualified Hob.Context              as HC
import qualified Hob.Context.UiContext    as HC

import Test.Hspec

import HobTest.Context.Default

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "focus sidebar" $
    it "focuses the sidebar" $ do
      ctx <- loadDefaultContext
      focused <- focusSidebarAndReturnState ctx
      focused `shouldBe` True

focusSidebarAndReturnState :: HC.Context -> IO Bool
focusSidebarAndReturnState ctx = do
    commandExecute focusSidebarCommandHandler ctx
    widgetGetIsFocus $ HC.sidebarTree . HC.uiContext $ ctx
