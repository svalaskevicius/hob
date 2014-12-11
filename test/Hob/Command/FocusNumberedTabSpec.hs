module Hob.Command.FocusNumberedTabSpec (main, spec) where

import Control.Monad   (replicateM_)
import Graphics.UI.Gtk

import           Hob.Command.FocusNumberedTab
import           Hob.Command.NewTab
import           Hob.Context
import qualified Hob.Context.UiContext        as HC

import Test.Hspec

import HobTest.Context.Default
import HobTest.Control

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "focus numbered tab command" $ do
    it "does not change the state if there is no matching tab" $ do
      ctx <- loadGuiWithNTabs 0
      currentPage <- focusNumberedTab 2 ctx
      currentPage `shouldBe` -1

    it "focuses the numbered tab" $ do
      ctx <- loadGuiWithNTabs 3
      notebookSetCurrentPage (HC.mainNotebook . uiContext $ ctx) 0
      currentPage <- focusNumberedTab 2 ctx
      currentPage `shouldBe` 2

focusNumberedTab :: Int -> Context -> IO Int
focusNumberedTab nr ctx = do
    runCtxActions ctx $ commandExecute (focusNumberedTabCommandHandler nr)
    let notebook = HC.mainNotebook . uiContext $ ctx
    notebookGetCurrentPage notebook

loadGuiWithNTabs :: Int -> IO Context
loadGuiWithNTabs n = do
    ctx <- loadDefaultContext
    replicateM_ n $ runCtxActions ctx editNewFile
    return ctx
