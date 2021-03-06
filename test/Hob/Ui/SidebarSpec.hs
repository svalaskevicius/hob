module Hob.Ui.SidebarSpec (main, spec) where

import Data.Maybe
import Data.Text       (unpack)
import Graphics.UI.Gtk

import qualified Hob.Context           as HC
import qualified Hob.Context.UiContext as HC
import           Hob.Control
import           Hob.Ui.Editor

import Test.Hspec

import HobTest.Context.Default
import HobTest.Context.Stubbed

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "sidebar" $ do
    it "is named" $ do
      ctx <- loadDefaultContext
      name <- widgetGetName $ HC.sidebarTree . HC.uiContext $ ctx
      name `shouldBe` "directoryListing"

    it "opens a file editor" $ do
      ctx <- loadStubbedContext
      activateDirectoryPath ctx [2]
      flushEvents
      editorText <- getActiveEditorText ctx
      (unpack . fromJust $ editorText) `shouldBe` "file contents for /xxx/c"

    it "does not open a file editor for directory" $ do
      ctx <- loadStubbedContext
      activateDirectoryPath ctx [0]
      pagesAfterActivatingDirectory <- getNumberOfEditorPages ctx
      pagesAfterActivatingDirectory `shouldBe` 0

    it "expands subtree on directory activation" $ do
      ctx <- loadStubbedContext
      activateDirectoryPath ctx [0]
      expanded <- treeViewRowExpanded (HC.sidebarTree . HC.uiContext $ ctx) [0]
      expanded `shouldBe` True

    it "does not open a file editor for files it cannot read" $ do
      ctx <- loadStubbedContext
      activateDirectoryPath ctx [3]
      pagesAfterActivatingDirectory <- getNumberOfEditorPages ctx
      pagesAfterActivatingDirectory `shouldBe` 0

activateDirectoryPath :: HC.Context -> TreePath -> IO ()
activateDirectoryPath ctx path = do
    let treeView = HC.sidebarTree . HC.uiContext $ ctx
    firstColumn <- treeViewGetColumn treeView 0
    treeViewRowActivated treeView path $ fromJust firstColumn


getNumberOfEditorPages :: HC.Context -> IO Int
getNumberOfEditorPages = notebookGetNPages . HC.mainNotebook . HC.uiContext
