module Hob.UiSpec (main, spec) where

import Control.Monad.Error
import Data.IORef
import Data.Maybe
import Data.Text                  (Text, pack, unpack)
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
      pagesAfterActivatingDirectory <- getNumberOfEditorPages mainWindow
      pagesAfterActivatingDirectory `shouldBe` 0

    it "does not open a file editor for files it cannot read" $ do
      mainWindow <- loadGui fileTreeStub stubbedFileLoader failingFileWriter
      activateDirectoryPath mainWindow [2]
      pagesAfterActivatingDirectory <- getNumberOfEditorPages mainWindow
      pagesAfterActivatingDirectory `shouldBe` 0

  describe "edit area" $ do
    it "does not allow to undo the intial loaded source" $ do
      mainWindow <- loadGui fileTreeStub stubbedFileLoader failingFileWriter
      tabbed <- getActiveEditorNotebook mainWindow
      editor <- launchNewEditorForText tabbed Nothing $ pack "initial text"
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
      (mainWindow, _) <- launchNewFileAndSetModified
      tabText <- getActiveEditorTabText mainWindow
      tabText `shouldBe` "testName.hs*"

    it "focuses the tab with the open file if requested to open an already loaded file" $ do
      mainWindow <- loadGui fileTreeStub stubbedFileLoader failingFileWriter
      notebook <- getActiveEditorNotebook mainWindow
      launchStubbedEditorTab mainWindow "/xxx/testName.hs"
      currentPageOfFirstLoadedFile <- notebookGetCurrentPage notebook
      launchStubbedEditorTab mainWindow "/xxx/c"
      pagesBeforeOpeningExistingFile <- notebookGetNPages notebook
      launchStubbedEditorTab mainWindow "/xxx/testName.hs"
      currentPageAfterLoadingTheFirstLoadedFile <- notebookGetCurrentPage notebook
      pagesAfterOpeningExistingFile <- notebookGetNPages notebook
      pagesAfterOpeningExistingFile `shouldBe` pagesBeforeOpeningExistingFile
      currentPageAfterLoadingTheFirstLoadedFile `shouldBe` currentPageOfFirstLoadedFile

  describe "editor commands" $ do
    it "closes the currently active editor tab" $ do
      mainWindow <- loadGui fileTreeStub stubbedFileLoader failingFileWriter
      launchStubbedEditorTab mainWindow "/xxx/testName.hs"
      closeCurrentEditorTab mainWindow
      pagesAfterActivatingDirectory <- getNumberOfEditorPages mainWindow
      pagesAfterActivatingDirectory `shouldBe` 0

    it "saves the currently active file" $ do
      (mockedWriter, mockReader) <- mockedFileWriter
      mainWindow <- loadGui fileTreeStub stubbedFileLoader failingFileWriter
      launchStubbedEditorTab mainWindow "/xxx/testName.hs"
      saveCurrentEditorTab emptyFileChooser mockedWriter mainWindow

      savedFile <- mockReader
      savedFile `shouldBe` Just ("/xxx/testName.hs", pack "file contents for /xxx/testName.hs")

    it "skips save when there is no active file" $ do
      mainWindow <- loadGui fileTreeStub stubbedFileLoader failingFileWriter
      saveCurrentEditorTab emptyFileChooser failingFileWriter mainWindow

    it "marks buffer as unmodified on save" $ do
      (mainWindow, buffer) <- launchNewFileAndSetModified
      saveCurrentEditorTab emptyFileChooser blackholeFileWriter mainWindow
      stateAfterSave <- textBufferGetModified buffer
      stateAfterSave `shouldBe` False

    it "requests filename for a new file" $ do
      mainWindow <- loadGui fileTreeStub stubbedFileLoader failingFileWriter
      editNewFile mainWindow
      (mockedWriter, mockReader) <- mockedFileWriter
      saveCurrentEditorTab (stubbedFileChooser $ Just "/xxx/fileResponded.hs") mockedWriter mainWindow
      savedFile <- mockReader
      savedFile `shouldBe` Just ("/xxx/fileResponded.hs", pack "")
      tabText <- getActiveEditorTabText mainWindow
      tabText `shouldBe` "fileResponded.hs"

    it "creates a new unnamed file" $ do
      mainWindow <- loadGui fileTreeStub stubbedFileLoader failingFileWriter
      editNewFile mainWindow
      pagesAfterActivatingDirectory <- getNumberOfEditorPages mainWindow
      pagesAfterActivatingDirectory `shouldBe` 1


launchNewFileAndSetModified :: IO (Window, TextBuffer)
launchNewFileAndSetModified = do
      mainWindow <- loadGui fileTreeStub stubbedFileLoader failingFileWriter
      launchStubbedEditorTab mainWindow "/xxx/testName.hs"
      buffer <- textViewGetBuffer . fromJust <=< getActiveEditor $ mainWindow
      textBufferSetModified buffer True
      return (mainWindow, buffer)

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
      text <- notebookGetTabLabelText tabbed $ fromJust currentlyActiveEditor
      return $ fromJust text

getNumberOfEditorPages :: Window -> IO Int
getNumberOfEditorPages = notebookGetNPages <=< getActiveEditorNotebook


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

blackholeFileWriter :: FileWriter
blackholeFileWriter _ _ = return ()

mockedFileWriter :: IO (FileWriter, IO (Maybe (String, Text)))
mockedFileWriter = do
      recorder <- newIORef Nothing
      return (curry $ (writeIORef recorder) . Just, readIORef recorder)

stubbedFileLoader :: FileLoader
stubbedFileLoader "/xxx/c" = return $ Just $ pack "file contents for /xxx/c"
stubbedFileLoader "/xxx/cannotRead" = return Nothing
stubbedFileLoader "/xxx/testName.hs" = return $ Just $ pack "file contents for /xxx/testName.hs"
stubbedFileLoader path = throwError $ userError $ "cannot open unknown file: "++path

stubbedFileChooser :: Maybe FilePath -> NewFileNameChooser
stubbedFileChooser = return

emptyFileChooser :: NewFileNameChooser
emptyFileChooser = stubbedFileChooser Nothing 