module Hob.UiSpec (main, spec) where

import Control.Monad.Error
import Data.IORef
import Data.Maybe
import Data.Text                            (Text, pack, unpack)
import Data.Tree
import Graphics.UI.Gtk
import Graphics.UI.Gtk.General.StyleContext
import Graphics.UI.Gtk.SourceView           (castToSourceBuffer,
                                             sourceBufferUndo)
import Hob.Context
import Hob.DirectoryTree
import Hob.Ui
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "mainWindow" $
    it "is named" $ do
      (mainWindow, _) <- loadDefaultGui
      name <- widgetGetName mainWindow
      name `shouldBe` "mainWindow"

  describe "sidebar" $ do
    it "is named" $ do
      (mainWindow, _) <- loadDefaultGui
      name <- widgetGetName =<< getDirectoryListingSidebar mainWindow
      name `shouldBe` "directoryListing"

    it "opens a file editor" $ do
      (mainWindow, _) <- loadDefaultGui
      activateDirectoryPath mainWindow [1]
      editorText <- getActiveEditorText mainWindow
      (unpack . fromJust $ editorText) `shouldBe` "file contents for /xxx/c"

    it "does not open a file editor for directory" $ do
      (mainWindow, _) <- loadDefaultGui
      activateDirectoryPath mainWindow [0]
      pagesAfterActivatingDirectory <- getNumberOfEditorPages mainWindow
      pagesAfterActivatingDirectory `shouldBe` 0

    it "does not open a file editor for files it cannot read" $ do
      (mainWindow, _) <- loadDefaultGui
      activateDirectoryPath mainWindow [2]
      pagesAfterActivatingDirectory <- getNumberOfEditorPages mainWindow
      pagesAfterActivatingDirectory `shouldBe` 0

  describe "command entry" $ do
    it "is named" $ do
      (mainWindow, _) <- loadDefaultGui
      name <- widgetGetName =<< getCommandEntry mainWindow
      name `shouldBe` "commandEntry"

    it "can be focused" $ do
      (mainWindow, _) <- loadDefaultGui
      focusCommandEntry mainWindow
      focused <- widgetGetIsFocus =<< getCommandEntry mainWindow
      focused `shouldBe` True

    it "initially there is no error class applied" $ do
      (mainWindow, _) <- loadDefaultGui
      styleContext <- widgetGetStyleContext =<< getCommandEntry mainWindow
      hasErrorClass <- styleContextHasClass styleContext "error"
      hasErrorClass `shouldBe` False

    it "applies error style class if the command is unknown" $ do
      (mainWindow, _) <- loadDefaultGui
      commandEntry <- getCommandEntry mainWindow
      entrySetText commandEntry "qweqwe"
      styleContext <- widgetGetStyleContext commandEntry
      hasErrorClass <- styleContextHasClass styleContext "error"
      hasErrorClass `shouldBe` True

    it "removes error style on empty command" $ do
      (_, commandEntry, styleContext) <- loadDefaultGuiWithCommandAndItsStyleContext
      entrySetText commandEntry "not empty"
      styleContextAddClass styleContext "error"
      entrySetText commandEntry ""
      hasErrorClass <- styleContextHasClass styleContext "error"
      hasErrorClass `shouldBe` False

    it "removes error style on search command" $ do
      (_, commandEntry, styleContext) <- loadDefaultGuiWithCommandAndItsStyleContext
      styleContextAddClass styleContext "error"
      entrySetText commandEntry "/asd"
      hasErrorClass <- styleContextHasClass styleContext "error"
      hasErrorClass `shouldBe` False

  describe "edit area" $ do
    it "does not allow to undo the intial loaded source" $ do
      (mainWindow, ctx) <- loadDefaultGui
      tabbed <- getActiveEditorNotebook mainWindow
      editor <- launchNewEditorForText ctx tabbed Nothing $ pack "initial text"
      buffer <- textViewGetBuffer editor
      sourceBufferUndo $ castToSourceBuffer buffer
      editorText <- getEditorText editor
      unpack editorText `shouldBe` "initial text"

    it "sets the tab title when opening a file" $ do
      (mainWindow, ctx) <- loadDefaultGui
      launchStubbedEditorTab mainWindow ctx "/xxx/testName.hs"
      tabText <- getActiveEditorTabText mainWindow
      tabText `shouldBe` "testName.hs"

    it "updates the tab title to reflect if buffer is modified" $ do
      (mainWindow, _) <- launchNewFileAndSetModified
      tabText <- getActiveEditorTabText mainWindow
      tabText `shouldBe` "testName.hs*"

    it "focuses the tab with the open file if requested to open an already loaded file" $ do
      (mainWindow, ctx) <- loadDefaultGui
      notebook <- getActiveEditorNotebook mainWindow
      launchStubbedEditorTab mainWindow ctx "/xxx/testName.hs"
      currentPageOfFirstLoadedFile <- notebookGetCurrentPage notebook
      launchStubbedEditorTab mainWindow ctx "/xxx/c"
      pagesBeforeOpeningExistingFile <- notebookGetNPages notebook
      launchStubbedEditorTab mainWindow ctx "/xxx/testName.hs"
      currentPageAfterLoadingTheFirstLoadedFile <- notebookGetCurrentPage notebook
      pagesAfterOpeningExistingFile <- notebookGetNPages notebook
      pagesAfterOpeningExistingFile `shouldBe` pagesBeforeOpeningExistingFile
      currentPageAfterLoadingTheFirstLoadedFile `shouldBe` currentPageOfFirstLoadedFile

  describe "text search command" $ do
    it "creates search tag on preview" $ do
      (_, buffer) <- loadGuiAndPreviewSearch
      tagTable <- textBufferGetTagTable buffer
      tag <- textTagTableLookup tagTable "search"
      isNothing tag `shouldBe` False


    it "applies search tag for matches on preview" $ do
      (_, buffer) <- loadGuiAndPreviewSearch
      tagStates <- checkSearchPreviewTagsAtRanges buffer [(0, 4), (15, 19)]
      tagStates `shouldBe` [(True, True), (True, True)]

    it "removes search tag from previous search on preview" $ do
      (mainWindow, buffer) <- loadGuiAndPreviewSearch
      searchPreview mainWindow "!"
      tagStates <- checkSearchPreviewTagsAtRanges buffer [(0, 4), (15, 19)]
      tagStates `shouldBe` [(False, False), (False, False)]

    it "removes all serch tags on reset" $ do
      (mainWindow, buffer) <- loadGuiAndPreviewSearch
      searchReset mainWindow
      tagStates <- checkSearchPreviewTagsAtRanges buffer [(0, 4), (15, 19)]
      tagStates `shouldBe` [(False, False), (False, False)]

  describe "editor commands" $ do
    it "closes the currently active editor tab" $ do
      (mainWindow, ctx) <- loadDefaultGui
      launchStubbedEditorTab mainWindow ctx "/xxx/testName.hs"
      closeCurrentEditorTab mainWindow
      pagesAfterActivatingDirectory <- getNumberOfEditorPages mainWindow
      pagesAfterActivatingDirectory `shouldBe` 0

    it "saves the currently active file" $ do
      (mockedWriter, mockReader) <- mockedFileWriter
      (mainWindow, ctx) <- loadDefaultGui
      launchStubbedEditorTab mainWindow ctx "/xxx/testName.hs"
      saveCurrentEditorTab emptyFileChooser mockedWriter mainWindow

      savedFile <- mockReader
      savedFile `shouldBe` Just ("/xxx/testName.hs", pack "file contents for /xxx/testName.hs")

    it "skips save when there is no active file" $ do
      (mainWindow, _) <- loadDefaultGui
      saveCurrentEditorTab emptyFileChooser failingFileWriter mainWindow

    it "marks buffer as unmodified on save" $ do
      (mainWindow, buffer) <- launchNewFileAndSetModified
      saveCurrentEditorTab emptyFileChooser blackholeFileWriter mainWindow
      stateAfterSave <- textBufferGetModified buffer
      stateAfterSave `shouldBe` False

    it "creates a new unnamed file" $ do
      mainWindow <- launchNewFile
      pagesAfterActivatingDirectory <- getNumberOfEditorPages mainWindow
      pagesAfterActivatingDirectory `shouldBe` 1

    it "requests filename for a new file" $ do
      mainWindow <- launchNewFile
      (mockedWriter, mockReader) <- mockedFileWriter
      saveCurrentEditorTab (stubbedFileChooser $ Just "/xxx/fileResponded.hs") mockedWriter mainWindow
      savedFile <- mockReader
      savedFile `shouldBe` Just ("/xxx/fileResponded.hs", pack "")
      tabText <- getActiveEditorTabText mainWindow
      tabText `shouldBe` "fileResponded.hs"

    it "updates the tab title from the newly got filepath" $ do
      mainWindow <- launchNewFile
      saveCurrentEditorTab (stubbedFileChooser $ Just "/xxx/fileResponded.hs") blackholeFileWriter mainWindow
      buffer <- textViewGetBuffer . fromJust <=< getActiveEditor $ mainWindow
      textBufferSetModified buffer True
      tabText <- getActiveEditorTabText mainWindow
      tabText `shouldBe` "fileResponded.hs*"

launchNewFile :: IO Window
launchNewFile = do
    (mainWindow, ctx) <- loadDefaultGui
    editNewFile ctx mainWindow
    return mainWindow

launchNewFileAndSetModified :: IO (Window, TextBuffer)
launchNewFileAndSetModified = do
    (mainWindow, ctx) <- loadDefaultGui
    launchStubbedEditorTab mainWindow ctx "/xxx/testName.hs"
    buffer <- textViewGetBuffer . fromJust <=< getActiveEditor $ mainWindow
    textBufferSetModified buffer True
    return (mainWindow, buffer)

launchStubbedEditorTab :: Window -> Context -> String -> IO ()
launchStubbedEditorTab mainWindow ctx file = do
    tabbed <- getActiveEditorNotebook mainWindow
    launchNewFileEditor ctx stubbedFileLoader tabbed file

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
fileTreeStub = return [
    Node (DirectoryTreeElement "a" "/xxx/a" True) [
        Node (DirectoryTreeElement "b" "/xxx/a/b" False) []],
    Node (DirectoryTreeElement "c" "/xxx/c" False) [],
    Node (DirectoryTreeElement "-" "/xxx/cannotRead" False) []]

failingFileWriter :: FileWriter
failingFileWriter _ _ = throwError $ userError "cannot write files stub"

blackholeFileWriter :: FileWriter
blackholeFileWriter _ _ = return ()

mockedFileWriter :: IO (FileWriter, IO (Maybe (String, Text)))
mockedFileWriter = do
    recorder <- newIORef Nothing
    return (curry $ writeIORef recorder . Just, readIORef recorder)

stubbedFileLoader :: FileLoader
stubbedFileLoader "/xxx/c" = return $ Just $ pack "file contents for /xxx/c"
stubbedFileLoader "/xxx/cannotRead" = return Nothing
stubbedFileLoader "/xxx/testName.hs" = return $ Just $ pack "file contents for /xxx/testName.hs"
stubbedFileLoader path = throwError $ userError $ "cannot open unknown file: "++path

stubbedFileChooser :: Maybe FilePath -> NewFileNameChooser
stubbedFileChooser = return

emptyFileChooser :: NewFileNameChooser
emptyFileChooser = stubbedFileChooser Nothing

stubbedCtx :: IO Context
stubbedCtx = defaultContext "app-data"

loadDefaultGui :: IO (Window, Context)
loadDefaultGui = do
    ctx <- stubbedCtx
    mainWindow <- loadGui ctx fileTreeStub stubbedFileLoader failingFileWriter
    return (mainWindow, ctx)

loadDefaultGuiWithCommandAndItsStyleContext :: IO (Window, Entry, StyleContext)
loadDefaultGuiWithCommandAndItsStyleContext = do
    (mainWindow, _) <- loadDefaultGui
    commandEntry <- getCommandEntry mainWindow
    styleContext <- widgetGetStyleContext commandEntry
    return (mainWindow, commandEntry, styleContext)

loadGuiAndPreviewSearch :: IO (Window, TextBuffer)
loadGuiAndPreviewSearch = do
    (mainWindow, ctx) <- loadDefaultGui
    tabbed <- getActiveEditorNotebook mainWindow
    editor <- launchNewEditorForText ctx tabbed Nothing $ pack "text - initial text!"
    searchPreview mainWindow "text"
    buffer <- textViewGetBuffer editor
    return (mainWindow, buffer)

checkSearchPreviewTagsAtRanges :: TextBuffer -> [(Int, Int)] -> IO [(Bool, Bool)]
checkSearchPreviewTagsAtRanges buffer ranges = do
      tagTable <- textBufferGetTagTable buffer
      tag <- textTagTableLookup tagTable "search"
      mapM (checkPair tag) ranges
      where
          checkPair tag (l, r) = do
              iL <- textBufferGetIterAtOffset buffer l
              iR <- textBufferGetIterAtOffset buffer r
              cL <- textIterBeginsTag iL tag
              cR <- textIterEndsTag iR tag
              return (cL, cR)
