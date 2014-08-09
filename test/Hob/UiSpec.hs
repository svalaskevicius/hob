module Hob.UiSpec where

import Control.Monad.Error
import Data.Maybe
import Data.Tree
import Graphics.UI.Gtk
import Hob.Ui
import Test.Hspec

main :: IO ()
main = hspec spec

fileTreeStub :: IO (Forest DirectoryTreeElement)
fileTreeStub = do
        return $ [
                Node (DirectoryTreeElement "a" "/xxx/a") [
                        Node (DirectoryTreeElement "b" "/xxx/a/b") []],
                Node (DirectoryTreeElement "c" "/xxx/c") []]

failingFileLoader :: FileLoader
failingFileLoader _ = throwError $ userError "cannot open files stub"

stubbedFileLoader :: FileLoader
stubbedFileLoader "/xxx/c" = return "file contents for /xxx/c"
stubbedFileLoader path = throwError $ userError $ "cannot open unknown file: "++path


spec :: Spec
spec = do
  describe "mainWindow" $ do
    it "mainWindow is named" $ do
      mainWindow <- loadGui fileTreeStub failingFileLoader
      name <- widgetGetName mainWindow
      name `shouldBe` "mainWindow"
    it "contains named sidebar" $ do
      mainWindow <- loadGui fileTreeStub failingFileLoader
      paned <- binGetChild mainWindow
      scrollbar <- panedGetChild1 $ castToPaned $ fromJust paned
      sidebar <- binGetChild $ castToScrolledWindow $ fromJust scrollbar
      name <- widgetGetName $ fromJust sidebar
      name `shouldBe` "directoryListing"

  describe "sidebar" $ do
    it "opens a file editor" $ do
      mainWindow <- loadGui fileTreeStub stubbedFileLoader
      paned <- binGetChild mainWindow
      scrollbar <- panedGetChild1 $ castToPaned $ fromJust paned
      sidebar <- binGetChild $ castToScrolledWindow $ fromJust scrollbar
      let treeView = castToTreeView $ fromJust sidebar
      firstColumn <- treeViewGetColumn treeView 0
      treeViewRowActivated treeView [1] $ fromJust firstColumn

      tabbed' <- panedGetChild2 $ castToPaned $ fromJust paned
      let tabbed = castToNotebook $ fromJust tabbed'
      pageNum <- notebookGetCurrentPage tabbed
      tabs <- containerGetChildren tabbed
      let currentlyActiveEditor = tabs!!pageNum
      let textEditScroller = castToScrolledWindow currentlyActiveEditor
      textEdit' <- binGetChild textEditScroller
      let textEdit = castToTextView $ fromJust textEdit'
      textBuf <- textViewGetBuffer textEdit
      editorText <- get textBuf textBufferText
      editorText `shouldBe` "file contents for /xxx/c"



