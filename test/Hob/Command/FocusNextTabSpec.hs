module Hob.Command.FocusNextTabSpec (main, spec) where

import Control.Monad   (replicateM_)
import Graphics.UI.Gtk

import           Hob.Command.FocusNextTab
import           Hob.Command.NewTab
import           Hob.Context
import qualified Hob.Context.UiContext    as HC

import Test.Hspec

import HobTest.Context.Default

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "focus next tab command" $ do
    it "does not change the state if there are no tabs" $ do
      ctx <- loadGuiWithNTabs 0
      currentPage <- focusNextTab ctx
      currentPage `shouldBe` -1

    it "leaves the first tab focused if there is just one tab" $ do
      ctx <- loadGuiWithNTabs 1
      currentPage <- focusNextTab ctx
      currentPage `shouldBe` 0

    it "focuses the next tab" $ do
      ctx <- loadGuiWithNTabs 2
      notebookSetCurrentPage (HC.mainNotebook . uiContext $ ctx) 0
      currentPage <- focusNextTab ctx
      currentPage `shouldBe` 1

    it "focuses the first tab if the command is invoked on the last tab" $ do
      ctx <- loadGuiWithNTabs 2
      notebookSetCurrentPage (HC.mainNotebook . uiContext $ ctx) 1
      currentPage <- focusNextTab ctx
      currentPage `shouldBe` 0

focusNextTab :: Context -> IO Int
focusNextTab ctx = do
    _ <- runApp (commandExecute focusNextTabCommandHandler) ctx
    let notebook = HC.mainNotebook . uiContext $ ctx
    notebookGetCurrentPage notebook

loadGuiWithNTabs :: Int -> IO Context
loadGuiWithNTabs n = do
    ctx <- loadDefaultContext
    replicateM_ n (runApp editNewFile ctx)
    return ctx
