module Hob.Ui.SidebarSpec (main, spec) where

import Data.Maybe
import Data.Text       (unpack)
import Graphics.UI.Gtk

import qualified Hob.Context as HC

import Hob.Ui.Editor

import Test.Hspec

import HobTest.Context.Default
import HobTest.Context.Stubbed

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "sidebar" $ do
    it "is named" $ do
      ctx <- loadDefaultContext
      name <- widgetGetName =<< getDirectoryListingSidebar ctx
      name `shouldBe` "directoryListing"

    it "opens a file editor" $ do
      ctx <- loadStubbedContext
      activateDirectoryPath ctx [1]
      editorText <- getActiveEditorText ctx
      (unpack . fromJust $ editorText) `shouldBe` "file contents for /xxx/c"

    it "does not open a file editor for directory" $ do
      ctx <- loadStubbedContext
      activateDirectoryPath ctx [0]
      pagesAfterActivatingDirectory <- getNumberOfEditorPages ctx
      pagesAfterActivatingDirectory `shouldBe` 0

    it "does not open a file editor for files it cannot read" $ do
      ctx <- loadStubbedContext
      activateDirectoryPath ctx [2]
      pagesAfterActivatingDirectory <- getNumberOfEditorPages ctx
      pagesAfterActivatingDirectory `shouldBe` 0

activateDirectoryPath :: HC.Context -> TreePath -> IO ()
activateDirectoryPath ctx path = do
    treeView <- getDirectoryListingSidebar ctx
    firstColumn <- treeViewGetColumn treeView 0
    treeViewRowActivated treeView path $ fromJust firstColumn


getDirectoryListingSidebar :: HC.Context -> IO TreeView
getDirectoryListingSidebar ctx = do
    paned <- binGetChild $ HC.mainWindow ctx
    scrollbar <- panedGetChild1 $ castToPaned $ fromJust paned
    sidebar <- binGetChild $ castToScrolledWindow $ fromJust scrollbar
    return (castToTreeView $ fromJust sidebar)

getNumberOfEditorPages :: HC.Context -> IO Int
getNumberOfEditorPages = notebookGetNPages . HC.mainNotebook
