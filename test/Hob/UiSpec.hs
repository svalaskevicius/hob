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

import qualified Hob.Context              as HC
import qualified Hob.Context.FileContext  as HFC
import qualified Hob.Context.StyleContext as HSC

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
      (mainWindow, _) <- loadStubbedGui
      activateDirectoryPath mainWindow [1]
      editorText <- getActiveEditorText mainWindow
      (unpack . fromJust $ editorText) `shouldBe` "file contents for /xxx/c"

    it "does not open a file editor for directory" $ do
      (mainWindow, _) <- loadStubbedGui
      activateDirectoryPath mainWindow [0]
      pagesAfterActivatingDirectory <- getNumberOfEditorPages mainWindow
      pagesAfterActivatingDirectory `shouldBe` 0

    it "does not open a file editor for files it cannot read" $ do
      (mainWindow, _) <- loadStubbedGui
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
      focused <- toggleFocusOnCommandEntryAndReturnState mainWindow
      focused `shouldBe` True

    it "focus stays on toggle if there is no editor to focus to" $ do
      (mainWindow, _) <- loadDefaultGui
      toggleFocusOnCommandEntry mainWindow
      focused <- toggleFocusOnCommandEntryAndReturnState mainWindow
      focused `shouldBe` True

    it "focus moves to editor on toggle" $ do
      mainWindow <- launchNewFile
      toggleFocusOnCommandEntry mainWindow
      commandFocused <- toggleFocusOnCommandEntryAndReturnState mainWindow
      editorFocused <- widgetGetIsFocus . fromJust =<< getActiveEditor mainWindow
      commandFocused `shouldBe` False
      editorFocused `shouldBe` True

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
      (mainWindow, ctx) <- loadStubbedGui
      launchEditorTab mainWindow ctx "/xxx/testName.hs"
      tabText <- getActiveEditorTabText mainWindow
      tabText `shouldBe` "testName.hs"

    it "updates the tab title to reflect if buffer is modified" $ do
      (mainWindow, _) <- launchNewFileAndSetModified
      tabText <- getActiveEditorTabText mainWindow
      tabText `shouldBe` "testName.hs*"

    it "focuses the tab with the open file if requested to open an already loaded file" $ do
      (mainWindow, ctx) <- loadStubbedGui
      notebook <- getActiveEditorNotebook mainWindow
      launchEditorTab mainWindow ctx "/xxx/testName.hs"
      currentPageOfFirstLoadedFile <- notebookGetCurrentPage notebook
      launchEditorTab mainWindow ctx "/xxx/c"
      pagesBeforeOpeningExistingFile <- notebookGetNPages notebook
      launchEditorTab mainWindow ctx "/xxx/testName.hs"
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

    it "removes all search tags on reset" $ do
      (mainWindow, buffer) <- loadGuiAndPreviewSearch
      searchReset mainWindow
      tagStates <- checkSearchPreviewTagsAtRanges buffer [(0, 4), (15, 19)]
      tagStates `shouldBe` [(False, False), (False, False)]

    it "highlights the first match on execute" $ do
      (_, buffer) <- loadGuiAndExecuteSearch
      (start, end) <- getSelectionOffsets buffer
      (start, end) `shouldBe` (0, 4)

    it "highlights the next match from cursor on execute" $ do
      (mainWindow, buffer) <- loadGuiAndExecuteSearch
      searchExecute mainWindow "text"
      (start, end) <- getSelectionOffsets buffer
      (start, end) `shouldBe` (15, 19)

    it "wraps the search from start if there are no matches till the end on execute" $ do
      (mainWindow, buffer) <- loadGuiAndExecuteSearch
      searchExecute mainWindow "text"
      searchExecute mainWindow "text"
      (start, end) <- getSelectionOffsets buffer
      (start, end) `shouldBe` (0, 4)

    it "scrolls to the current match on execute" $ do
      (mainWindow, ctx) <- loadDefaultGui
      tabbed <- getActiveEditorNotebook mainWindow
      let editorText = (concat . replicate 1000  $ "text - initial text! \n") ++ "customised search string at the end\n"
      editor <- launchNewEditorForText ctx tabbed Nothing $ pack editorText
      processGtkEvents
      searchExecute mainWindow "customised search string at the end"
      processGtkEvents
      buffer <- textViewGetBuffer editor
      visible <- textViewGetVisibleRect editor
      caretIter <- textBufferGetIterAtMark buffer =<< textBufferGetInsert buffer
      cursor <- textViewGetIterLocation editor caretIter
      isRectangleInside visible cursor `shouldBe` True

  describe "editor commands" $ do
    it "closes the currently active editor tab" $ do
      (mainWindow, ctx) <- loadStubbedGui
      launchEditorTab mainWindow ctx "/xxx/testName.hs"
      closeCurrentEditorTab mainWindow
      pagesAfterActivatingDirectory <- getNumberOfEditorPages mainWindow
      pagesAfterActivatingDirectory `shouldBe` 0

    it "saves the currently active file" $ do
      (mockedWriter, mockReader) <- mockedFileWriter
      sc <- HSC.defaultStyleContext "app-data"
      fc <- HFC.defaultFileContext stubbedFileLoader mockedWriter emptyFileTree
      let ctx = HC.Context sc fc
      mainWindow <- launchFileInContext ctx "/xxx/testName.hs"
      saveCurrentEditorTab ctx emptyFileChooser mainWindow
      savedFile <- mockReader
      savedFile `shouldBe` Just ("/xxx/testName.hs", pack "file contents for /xxx/testName.hs")

    it "skips save when there is no active file" $ do
      sc <- HSC.defaultStyleContext "app-data"
      fc <- HFC.defaultFileContext emptyFileLoader failingFileWriter emptyFileTree
      let ctx = HC.Context sc fc
      mainWindow <- loadGui ctx
      saveCurrentEditorTab ctx emptyFileChooser mainWindow

    it "marks buffer as unmodified on save" $ do
      sc <- HSC.defaultStyleContext "app-data"
      fc <- HFC.defaultFileContext stubbedFileLoader blackholeFileWriter emptyFileTree
      let ctx = HC.Context sc fc
      mainWindow <- launchFileInContext ctx "/xxx/testName.hs"
      buffer <- textViewGetBuffer . fromJust <=< getActiveEditor $ mainWindow
      textBufferSetModified buffer True
      saveCurrentEditorTab ctx emptyFileChooser mainWindow
      stateAfterSave <- textBufferGetModified buffer
      stateAfterSave `shouldBe` False

    it "creates a new unnamed file" $ do
      mainWindow <- launchNewFile
      pagesAfterActivatingDirectory <- getNumberOfEditorPages mainWindow
      pagesAfterActivatingDirectory `shouldBe` 1

    it "requests filename for a new file" $ do
      (mockedWriter, mockReader) <- mockedFileWriter
      sc <- HSC.defaultStyleContext "app-data"
      fc <- HFC.defaultFileContext emptyFileLoader mockedWriter emptyFileTree
      mainWindow <- launchNewFileInContextAndSaveAs (HC.Context sc fc) "/xxx/fileResponded.hs"
      savedFile <- mockReader
      savedFile `shouldBe` Just ("/xxx/fileResponded.hs", pack "")
      tabText <- getActiveEditorTabText mainWindow
      tabText `shouldBe` "fileResponded.hs"

    it "updates the tab title from the newly got filepath" $ do
      sc <- HSC.defaultStyleContext "app-data"
      fc <- HFC.defaultFileContext emptyFileLoader blackholeFileWriter emptyFileTree
      mainWindow <- launchNewFileInContextAndSaveAs (HC.Context sc fc) "/xxx/fileResponded.hs"
      buffer <- textViewGetBuffer . fromJust <=< getActiveEditor $ mainWindow
      textBufferSetModified buffer True
      tabText <- getActiveEditorTabText mainWindow
      tabText `shouldBe` "fileResponded.hs*"

launchNewFile :: IO Window
launchNewFile = do
    (mainWindow, ctx) <- loadDefaultGui
    editNewFile ctx mainWindow
    return mainWindow

launchFileInContext :: HC.Context -> String -> IO Window
launchFileInContext ctx filename = do
      mainWindow <- loadGui ctx
      launchEditorTab mainWindow ctx filename
      return mainWindow

launchNewFileInContextAndSaveAs :: HC.Context -> String -> IO Window
launchNewFileInContextAndSaveAs ctx filename = do
      mainWindow <- loadGui ctx
      editNewFile ctx mainWindow
      saveCurrentEditorTab ctx (stubbedFileChooser $ Just filename) mainWindow
      return mainWindow

launchNewFileAndSetModified :: IO (Window, TextBuffer)
launchNewFileAndSetModified = do
    (mainWindow, ctx) <- loadStubbedGui
    launchEditorTab mainWindow ctx "/xxx/testName.hs"
    buffer <- textViewGetBuffer . fromJust <=< getActiveEditor $ mainWindow
    textBufferSetModified buffer True
    return (mainWindow, buffer)

launchEditorTab :: Window -> HC.Context -> String -> IO ()
launchEditorTab mainWindow ctx file = do
    tabbed <- getActiveEditorNotebook mainWindow
    launchNewFileEditor ctx tabbed file

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

failingFileWriter :: HFC.FileWriter
failingFileWriter _ _ = throwError $ userError "cannot write files stub"

mockedFileWriter :: IO (HFC.FileWriter, IO (Maybe (String, Text)))
mockedFileWriter = do
    recorder <- newIORef Nothing
    return (curry $ writeIORef recorder . Just, readIORef recorder)

stubbedFileLoader :: HFC.FileLoader
stubbedFileLoader "/xxx/c" = return $ Just $ pack "file contents for /xxx/c"
stubbedFileLoader "/xxx/cannotRead" = return Nothing
stubbedFileLoader "/xxx/testName.hs" = return $ Just $ pack "file contents for /xxx/testName.hs"
stubbedFileLoader path = throwError $ userError $ "cannot open unknown file: "++path

stubbedFileChooser :: Maybe FilePath -> NewFileNameChooser
stubbedFileChooser = return

emptyFileChooser :: NewFileNameChooser
emptyFileChooser = stubbedFileChooser Nothing

defaultCtx :: IO HC.Context
defaultCtx = do
    sc <- HSC.defaultStyleContext "app-data"
    fc <- HFC.defaultFileContext emptyFileLoader blackholeFileWriter emptyFileTree
    return $ HC.Context sc fc


stubbedCtx :: IO HC.Context
stubbedCtx = do
    sc <- HSC.defaultStyleContext "app-data"
    fc <- HFC.defaultFileContext stubbedFileLoader failingFileWriter fileTreeStub
    return $ HC.Context sc fc

blackholeFileWriter :: HFC.FileWriter
blackholeFileWriter _ _ = return ()

emptyFileTree :: HFC.FileTreeLoader
emptyFileTree = return []

emptyFileLoader :: HFC.FileLoader
emptyFileLoader _ = return $ Just $ pack ""

loadDefaultGui :: IO (Window, HC.Context)
loadDefaultGui = do
    ctx <- defaultCtx
    mainWindow <- loadGui ctx
    return (mainWindow, ctx)

loadStubbedGui :: IO (Window, HC.Context)
loadStubbedGui = do
    ctx <- stubbedCtx
    mainWindow <- loadGui ctx
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

loadGuiAndExecuteSearch :: IO (Window, TextBuffer)
loadGuiAndExecuteSearch = do
    (mainWindow, buffer) <- loadGuiAndPreviewSearch
    iterBufferStart <- textBufferGetIterAtOffset buffer 0
    textBufferSelectRange buffer iterBufferStart iterBufferStart
    searchExecute mainWindow "text"
    return (mainWindow, buffer)

getSelectionOffsets :: TextBuffer -> IO (Int, Int)
getSelectionOffsets buffer = do
    (iterStart, iterEnd) <- textBufferGetSelectionBounds buffer
    start <- textIterGetOffset iterStart
    end <- textIterGetOffset iterEnd
    return (start, end)

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

toggleFocusOnCommandEntryAndReturnState :: Window -> IO Bool
toggleFocusOnCommandEntryAndReturnState mainWindow = do
    toggleFocusOnCommandEntry mainWindow
    widgetGetIsFocus =<< getCommandEntry mainWindow

isRectangleInside :: Rectangle -> Rectangle -> Bool
isRectangleInside (Rectangle ax ay aw ah) (Rectangle bx by bw bh) =
    (ax <= bx) && (ay <= by) && (ax+aw >= bx+bw) && (ay+ah >= by+bh)

processGtkEvents :: IO ()
processGtkEvents = replicateM_ 500 $ mainIterationDo False
