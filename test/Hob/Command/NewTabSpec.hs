module Hob.Command.NewTabSpec (main, spec) where

import Test.Hspec

import Graphics.UI.Gtk

import qualified Hob.Context           as HC
import qualified Hob.Context.UiContext as HC

import HobTest.Context.Stubbed
import HobTest.Editor

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "new tab command" $ do
    it "creates a new unnamed file" $ do
      ctx <- launchNewFile
      pagesAfterActivatingDirectory <- getNumberOfEditorPages ctx
      pagesAfterActivatingDirectory `shouldBe` 1

    it "focuses the tab with the open file if requested to open an already loaded file" $ do
      ctx <- loadStubbedContext
      let notebook = HC.mainNotebook . HC.uiContext $ ctx
      launchEditorTab ctx "/xxx/testName.hs"
      currentPageOfFirstLoadedFile <- notebookGetCurrentPage notebook
      launchEditorTab ctx "/xxx/c"
      pagesBeforeOpeningExistingFile <- notebookGetNPages notebook
      launchEditorTab ctx "/xxx/testName.hs"
      currentPageAfterLoadingTheFirstLoadedFile <- notebookGetCurrentPage notebook
      pagesAfterOpeningExistingFile <- notebookGetNPages notebook
      pagesAfterOpeningExistingFile `shouldBe` pagesBeforeOpeningExistingFile
      currentPageAfterLoadingTheFirstLoadedFile `shouldBe` currentPageOfFirstLoadedFile
