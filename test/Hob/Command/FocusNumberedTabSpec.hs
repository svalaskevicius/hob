module Hob.Command.FocusNumberedTabSpec (main, spec) where

import Control.Monad   (replicateM_)
import Graphics.UI.Gtk

import           Hob.Command
import           Hob.Command.FocusNumberedTab
import           Hob.Command.NewTab
import qualified Hob.Context                  as HC
import qualified Hob.Context.UiContext        as HC

import Test.Hspec

import HobTest.Context.Default

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
      notebookSetCurrentPage (HC.mainNotebook . HC.uiContext $ ctx) 0
      currentPage <- focusNumberedTab 2 ctx
      currentPage `shouldBe` 2

focusNumberedTab :: Int -> HC.Context -> IO Int
focusNumberedTab nr ctx = do
    commandExecute (focusNumberedTabCommandHandler nr) ctx
    let notebook = HC.mainNotebook . HC.uiContext $ ctx
    notebookGetCurrentPage notebook

loadGuiWithNTabs :: Int -> IO HC.Context
loadGuiWithNTabs n = do
    ctx <- loadDefaultContext
    replicateM_ n (editNewFile ctx)
    return ctx
