module Hob.Command.FocusSidebarSpec (main, spec) where

import Graphics.UI.Gtk

import           Hob.Command
import           Hob.Command.FocusSidebar
import qualified Hob.Context              as HC
import qualified Hob.Context.UiContext    as HC

import Test.Hspec

import HobTest.Context.Default
import HobTest.Context.Stubbed
import HobTest.Editor

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "focus sidebar" $ do
    it "focuses the sidebar" $ do
      ctx <- loadDefaultContext
      focused <- focusSidebarAndReturnState ctx
      focused `shouldBe` True

    it "can sync the open editor path in sidebar" $ do
      ctx <- loadStubbedContext
      launchEditorTab ctx "/xxx/a/b"
      path <- syncSidebarAndReturnSelectedPath ctx
      focused <- sidebarFocusState ctx
      path `shouldBe` [0, 0]
      focused `shouldBe` True


sidebarFocusState :: HC.Context -> IO Bool
sidebarFocusState = widgetGetIsFocus . HC.sidebarTree . HC.uiContext

focusSidebarAndReturnState :: HC.Context -> IO Bool
focusSidebarAndReturnState ctx = do
    commandExecute focusSidebarCommandHandler ctx
    sidebarFocusState ctx

syncSidebarAndReturnSelectedPath :: HC.Context -> IO TreePath
syncSidebarAndReturnSelectedPath ctx = do
    commandExecute syncFocusSidebarCommandHandler ctx
    pos <- treeViewGetCursor . HC.sidebarTree . HC.uiContext $ ctx
    return $ fst pos
