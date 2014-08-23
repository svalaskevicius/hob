module Hob.UiSpec where

import Control.Monad.Error
import Data.IORef
import Data.Maybe
import Data.Text                  (pack, unpack)
import Data.Tree
import Graphics.UI.Gtk
import Graphics.UI.Gtk.SourceView (castToSourceBuffer, sourceBufferUndo)
import Hob.DirectoryTree
import Hob.Ui
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "mainWindow" $ do
    it "mainWindow is named" $ do
      mainWindow <- loadGui fileTreeStub failingFileLoader failingFileWriter
      name <- widgetGetName mainWindow
      name `shouldBe` "mainWindow"

    it "contains named sidebar" $ do
      mainWindow <- loadGui fileTreeStub failingFileLoader failingFileWriter
      name <- widgetGetName =<< getDirectoryListingSidebar mainWindow
      name `shouldBe` "directoryListing"

  describe "sidebar" $ do
    it "opens a file editor" $ do
      mainWindow <- loadGui fileTreeStub stubbedFileLoader failingFileWriter
      activateDirectoryPath mainWindow [1]
      editorText <- getActiveEditorText mainWindow
      (unpack . fromJust $ editorText) `shouldBe` "file contents for /xxx/c"

    it "does not open a file editor for directory" $ do
      mainWindow <- loadGui fileTreeStub stubbedFileLoader failingFileWriter
      activateDirectoryPath mainWindow [0]
      isWelcome <- isShowingWelcome mainWindow
      isWelcome `shouldBe` True

    it "does not open a file editor for files it cannot read" $ do
      mainWindow <- loadGui fileTreeStub stubbedFileLoader failingFileWriter
      activateDirectoryPath mainWindow [2]
      isWelcome <- isShowingWelcome mainWindow
      isWelcome `shouldBe` True

  describe "edit area" $ do
    it "does not allow to undo the intial loaded source" $ do
      mainWindow <- loadGui fileTreeStub stubbedFileLoader failingFileWriter
      tabbed <- getActiveEditorNotebook mainWindow
      editor <- launchNewEditorForText tabbed "testfile" $ pack "initial text"
      buffer <- textViewGetBuffer editor
      sourceBufferUndo $ castToSourceBuffer buffer
      editorText <- getEditorText editor
      unpack editorText `shouldBe` "initial text"

    it "sets the tab title when opening a file" $ do
      mainWindow <- loadGui fileTreeStub stubbedFileLoader failingFileWriter
      launchStubbedEditorTab mainWindow "/xxx/testName.hs"
      tabText <- getActiveEditorTabText mainWindow
      tabText `shouldBe` "testName.hs"

    it "updates the tab title to reflect if buffer is modified" $ do
      mainWindow <- loadGui fileTreeStub stubbedFileLoader failingFileWriter
      launchStubbedEditorTab mainWindow "/xxx/testName.hs"
      buffer <- textViewGetBuffer . fromJust <=< getActiveEditor $ mainWindow
      textBufferSetModified buffer True
      tabText <- getActiveEditorTabText mainWindow
      tabText `shouldBe` "testName.hs*"

  describe "editor commands" $ do
    it "closes the currently active editor tab" $ do
      mainWindow <- loadGui fileTreeStub stubbedFileLoader failingFileWriter
      launchStubbedEditorTab mainWindow "/xxx/testName.hs"
      closeCurrentEditorTab mainWindow
      tabText <- getActiveEditorTabText mainWindow
      tabText `shouldBe` "Welcome"

    it "saves the currently active file" $ do
      recorder <- newIORef ("", pack "")
      let stubbedFileWriter = curry $ writeIORef recorder
      mainWindow <- loadGui fileTreeStub stubbedFileLoader failingFileWriter
      launchStubbedEditorTab mainWindow "/xxx/testName.hs"
      saveCurrentEditorTab stubbedFileWriter mainWindow

      savedFile <- readIORef recorder
      savedFile `shouldBe` ("/xxx/testName.hs", pack "file contents for /xxx/testName.hs")

    it "skips save when there is no active file" $ do
      mainWindow <- loadGui fileTreeStub stubbedFileLoader failingFileWriter
      saveCurrentEditorTab failingFileWriter mainWindow


launchStubbedEditorTab :: Window -> String -> IO ()
launchStubbedEditorTab mainWindow file = do
      tabbed <- getActiveEditorNotebook mainWindow
      launchNewFileEditor stubbedFileLoader tabbed file

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


getActiveEditorTabText :: Window -> IO String
getActiveEditorTabText mainWindow = do
      tabbed <- getActiveEditorNotebook mainWindow
      currentlyActiveEditor <- getActiveEditorTab mainWindow
      text <- notebookGetTabLabelText tabbed currentlyActiveEditor
      return $ fromJust text

isShowingWelcome :: Window -> IO Bool
isShowingWelcome mainWindow = do
      name <- widgetGetName =<< getActiveEditorTab mainWindow
      return (name == "welcomeText")


fileTreeStub :: IO (Forest DirectoryTreeElement)
fileTreeStub =
        return [
                Node (DirectoryTreeElement "a" "/xxx/a" True) [
                        Node (DirectoryTreeElement "b" "/xxx/a/b" False) []],
                Node (DirectoryTreeElement "c" "/xxx/c" False) [],
                Node (DirectoryTreeElement "-" "/xxx/cannotRead" False) []]

failingFileLoader :: FileLoader
failingFileLoader _ = throwError $ userError "cannot open files stub"

failingFileWriter :: FileWriter
failingFileWriter _ _ = throwError $ userError "cannot write files stub"

stubbedFileLoader :: FileLoader
stubbedFileLoader "/xxx/c" = return $ Just $ pack "file contents for /xxx/c"
stubbedFileLoader "/xxx/cannotRead" = return Nothing
stubbedFileLoader "/xxx/testName.hs" = return $ Just $ pack "file contents for /xxx/testName.hs"
stubbedFileLoader path = throwError $ userError $ "cannot open unknown file: "++path


