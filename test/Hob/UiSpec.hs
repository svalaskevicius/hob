module Hob.UiSpec where

import Control.Monad.Error
import Data.Maybe
import Data.Text           (pack)
import Data.Tree
import Graphics.UI.Gtk
import Hob.DirectoryTree
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

  describe "sidebar" $ do
    it "opens a file editor" $ do
      mainWindow <- loadGui fileTreeStub stubbedFileLoader
      activateDirectoryPath mainWindow [1]
      editorText <- getActiveEditorText mainWindow
      editorText `shouldBe` "file contents for /xxx/c"
    it "does not open a file editor for directory" $ do
      mainWindow <- loadGui fileTreeStub stubbedFileLoader
      activateDirectoryPath mainWindow [0]
      isWelcome <- isShowingWelcome mainWindow
      isWelcome `shouldBe` True
    it "does not open a file editor for files it cannot read" $ do
      mainWindow <- loadGui fileTreeStub stubbedFileLoader
      activateDirectoryPath mainWindow [2]
      isWelcome <- isShowingWelcome mainWindow
      isWelcome `shouldBe` True



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
      currentlyActiveEditor <- getActiveEditorTab mainWindow
      let textEditScroller = castToScrolledWindow currentlyActiveEditor
      textEdit' <- binGetChild textEditScroller
      return (castToTextView $ fromJust textEdit')

isShowingWelcome :: Window -> IO Bool
isShowingWelcome mainWindow = do
      name <- widgetGetName =<< getActiveEditorTab mainWindow
      return (name == "welcomeText")

getActiveEditorTab :: Window -> IO Widget
getActiveEditorTab mainWindow = do
      paned <- binGetChild mainWindow
      tabbed' <- panedGetChild2 $ castToPaned $ fromJust paned
      let tabbed = castToNotebook $ fromJust tabbed'
      pageNum <- notebookGetCurrentPage tabbed
      tabs <- containerGetChildren tabbed
      return (tabs!!pageNum)


fileTreeStub :: IO (Forest DirectoryTreeElement)
fileTreeStub =
        return [
                Node (DirectoryTreeElement "a" "/xxx/a" (IsDirectory True)) [
                        Node (DirectoryTreeElement "b" "/xxx/a/b" (IsDirectory False)) []],
                Node (DirectoryTreeElement "c" "/xxx/c" (IsDirectory False)) [],
                Node (DirectoryTreeElement "-" "/xxx/cannotRead" (IsDirectory False)) []]

failingFileLoader :: FileLoader
failingFileLoader _ = throwError $ userError "cannot open files stub"

stubbedFileLoader :: FileLoader
stubbedFileLoader "/xxx/c" = return $ Just $ pack "file contents for /xxx/c"
stubbedFileLoader "/xxx/cannotRead" = return Nothing
stubbedFileLoader path = throwError $ userError $ "cannot open unknown file: "++path


