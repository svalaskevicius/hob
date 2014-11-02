module Hob.Command.FocusPreviousTabSpec (main, spec) where

import Control.Monad   (replicateM_)
import Graphics.UI.Gtk

import           Hob.Command.FocusPreviousTab
import           Hob.Command.NewTab
import           Hob.Context
import qualified Hob.Context.UiContext        as HC

import Test.Hspec

import HobTest.Context.Default

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "focus previous tab command" $ do
    it "does not change the state if there are no tabs" $ do
      ctx <- loadGuiWithNTabs 0
      currentPage <- focusPreviousTab ctx
      currentPage `shouldBe` -1

    it "leaves the first tab focused if there is just one tab" $ do
      ctx <- loadGuiWithNTabs 1
      currentPage <- focusPreviousTab ctx
      currentPage `shouldBe` 0

    it "focuses the previous tab" $ do
      ctx <- loadGuiWithNTabs 2
      notebookSetCurrentPage (HC.mainNotebook . uiContext $ ctx) 1
      currentPage <- focusPreviousTab ctx
      currentPage `shouldBe` 0

    it "focuses the last tab if the command is invoked on the first tab" $ do
      ctx <- loadGuiWithNTabs 2
      notebookSetCurrentPage (HC.mainNotebook . uiContext $ ctx) 0
      currentPage <- focusPreviousTab ctx
      currentPage `shouldBe` 1

focusPreviousTab :: Context -> IO Int
focusPreviousTab ctx = do
    deferredRunner ctx $ commandExecute focusPreviousTabCommandHandler
    let notebook = HC.mainNotebook . uiContext $ ctx
    notebookGetCurrentPage notebook

loadGuiWithNTabs :: Int -> IO Context
loadGuiWithNTabs n = do
    ctx <- loadDefaultContext
    replicateM_ n $ deferredRunner ctx editNewFile
    return ctx
