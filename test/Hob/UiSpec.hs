module Hob.UiSpec where

import Control.Monad.Error
import Data.Maybe
import Data.Tree
import Graphics.UI.Gtk
import Hob.Ui
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "mainWindow" $ do
    it "mainWindow is named" $ do
      mainWindow <- loadGui fileTreeStub failingFileLoader
      name <- widgetGetName mainWindow
      name `shouldBe` "mainWindow"

    it "contains named sidebar" $ do
      mainWindow <- loadGui fileTreeStub failingFileLoader
      name <- widgetGetName =<< getDirectoryListingSidebar mainWindow
      name `shouldBe` "directoryListing"

  describe "sidebar" $
    it "opens a file editor" $ do
      mainWindow <- loadGui fileTreeStub stubbedFileLoader
      activateDirectoryPath mainWindow [1]
      editorText <- getActiveEditorText mainWindow
      editorText `shouldBe` "file contents for /xxx/c"


activateDirectoryPath :: Window -> TreePath -> IO ()
activateDirectoryPath mainWindow path = do
      treeView <- getDirectoryListingSidebar mainWindow
      firstColumn <- treeViewGetColumn treeView 0
      treeViewRowActivated treeView path $ fromJust firstColumn


getDirectoryListingSidebar :: Window -> IO TreeView
getDirectoryListingSidebar mainWindow = do
        paned <- binGetChild mainWindow
        scrollbar <- panedGetChild1 $ castToPaned $ fromJust paned
        sidebar <- binGetChild $ castToScrolledWindow $ fromJust scrollbar
        return (castToTreeView $ fromJust sidebar)

getActiveEditorText :: Window -> IO String
getActiveEditorText mainWindow = do
      textEdit <- getActiveEditor mainWindow
      textBuf <- textViewGetBuffer textEdit
      get textBuf textBufferText


getActiveEditor :: Window -> IO TextView
getActiveEditor mainWindow = do
      paned <- binGetChild mainWindow
      tabbed' <- panedGetChild2 $ castToPaned $ fromJust paned
      let tabbed = castToNotebook $ fromJust tabbed'
      pageNum <- notebookGetCurrentPage tabbed
      tabs <- containerGetChildren tabbed
      let currentlyActiveEditor = tabs!!pageNum
      let textEditScroller = castToScrolledWindow currentlyActiveEditor
      textEdit' <- binGetChild textEditScroller
      return (castToTextView $ fromJust textEdit')


fileTreeStub :: IO (Forest DirectoryTreeElement)
fileTreeStub =
        return [
                Node (DirectoryTreeElement "a" "/xxx/a") [
                        Node (DirectoryTreeElement "b" "/xxx/a/b") []],
                Node (DirectoryTreeElement "c" "/xxx/c") []]

failingFileLoader :: FileLoader
failingFileLoader _ = throwError $ userError "cannot open files stub"

stubbedFileLoader :: FileLoader
stubbedFileLoader "/xxx/c" = return "file contents for /xxx/c"
stubbedFileLoader path = throwError $ userError $ "cannot open unknown file: "++path


