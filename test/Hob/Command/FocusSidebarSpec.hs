module Hob.Command.FocusSidebarSpec (main, spec) where

import Graphics.UI.Gtk

import           Hob.Command.FocusSidebar
import           Hob.Context
import qualified Hob.Context.UiContext    as HC

import Test.Hspec

import HobTest.Context.Default
import HobTest.Context.Stubbed
import HobTest.Control
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

    it "can sync the open editor path in sidebar when there is a directory named as the file's basename" $ do
      ctx <- loadStubbedContext
      launchEditorTab ctx "/xxx/a.b"
      path <- syncSidebarAndReturnSelectedPath ctx
      focused <- sidebarFocusState ctx
      path `shouldBe` [1]
      focused `shouldBe` True

sidebarFocusState :: Context -> IO Bool
sidebarFocusState = widgetGetIsFocus . HC.sidebarTree . uiContext

focusSidebarAndReturnState :: Context -> IO Bool
focusSidebarAndReturnState ctx = do
    runCtxActions ctx $ commandExecute focusSidebarCommandHandler
    sidebarFocusState ctx

syncSidebarAndReturnSelectedPath :: Context -> IO TreePath
syncSidebarAndReturnSelectedPath ctx = do
    runCtxActions ctx $ commandExecute syncFocusSidebarCommandHandler
    pos <- treeViewGetCursor . HC.sidebarTree . uiContext $ ctx
    return $ fst pos
