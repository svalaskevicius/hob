module Hob.Command.NewTabSpec (main, spec) where

import Test.Hspec

import Graphics.UI.Gtk

import qualified Hob.Context as HC

import Hob.Command
import Hob.Command.NewTab

import HobTest.Context.Default
import HobTest.Context.Stubbed

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
      let notebook = HC.mainNotebook ctx
      launchEditorTab ctx "/xxx/testName.hs"
      currentPageOfFirstLoadedFile <- notebookGetCurrentPage notebook
      launchEditorTab ctx "/xxx/c"
      pagesBeforeOpeningExistingFile <- notebookGetNPages notebook
      launchEditorTab ctx "/xxx/testName.hs"
      currentPageAfterLoadingTheFirstLoadedFile <- notebookGetCurrentPage notebook
      pagesAfterOpeningExistingFile <- notebookGetNPages notebook
      pagesAfterOpeningExistingFile `shouldBe` pagesBeforeOpeningExistingFile
      currentPageAfterLoadingTheFirstLoadedFile `shouldBe` currentPageOfFirstLoadedFile


launchNewFile :: IO HC.Context
launchNewFile = do
    ctx <- loadDefaultContext
    commandExecute editNewFileCommandHandler ctx
    return ctx

launchEditorTab :: HC.Context -> String -> IO ()
launchEditorTab ctx file = do
    let notebook = HC.mainNotebook ctx
    launchNewFileEditor ctx notebook file

getNumberOfEditorPages :: HC.Context -> IO Int
getNumberOfEditorPages = notebookGetNPages . HC.mainNotebook

