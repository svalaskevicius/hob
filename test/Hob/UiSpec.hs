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
      ctx <- loadDefaultGui
      name <- widgetGetName $ HC.mainWindow ctx
      name `shouldBe` "mainWindow"

  describe "sidebar" $ do
    it "is named" $ do
      ctx <- loadDefaultGui
      name <- widgetGetName =<< getDirectoryListingSidebar ctx
      name `shouldBe` "directoryListing"

    it "opens a file editor" $ do
      ctx <- loadStubbedGui
      activateDirectoryPath ctx [1]
      editorText <- getActiveEditorText ctx
      (unpack . fromJust $ editorText) `shouldBe` "file contents for /xxx/c"

    it "does not open a file editor for directory" $ do
      ctx <- loadStubbedGui
      activateDirectoryPath ctx [0]
      pagesAfterActivatingDirectory <- getNumberOfEditorPages ctx
      pagesAfterActivatingDirectory `shouldBe` 0

    it "does not open a file editor for files it cannot read" $ do
      ctx <- loadStubbedGui
      activateDirectoryPath ctx [2]
      pagesAfterActivatingDirectory <- getNumberOfEditorPages ctx
      pagesAfterActivatingDirectory `shouldBe` 0

  describe "command entry" $ do
    it "is named" $ do
      ctx <- loadDefaultGui
      name <- widgetGetName $ HC.commandEntry ctx
      name `shouldBe` "commandEntry"

    it "can be focused" $ do
      ctx <- loadDefaultGui
      focused <- toggleFocusOnCommandEntryAndReturnState ctx
      focused `shouldBe` True

    it "focus stays on toggle if there is no editor to focus to" $ do
      ctx <- loadDefaultGui
      toggleFocusOnCommandEntry ctx
      focused <- toggleFocusOnCommandEntryAndReturnState ctx
      focused `shouldBe` True

    it "focus moves to editor on toggle" $ do
      ctx <- launchNewFile
      toggleFocusOnCommandEntry ctx
      commandFocused <- toggleFocusOnCommandEntryAndReturnState ctx
      editorFocused <- widgetGetIsFocus . fromJust =<< getActiveEditor ctx
      commandFocused `shouldBe` False
      editorFocused `shouldBe` True

    it "initially there is no error class applied" $ do
      ctx <- loadDefaultGui
      styleContext <- widgetGetStyleContext $ HC.commandEntry ctx
      hasErrorClass <- styleContextHasClass styleContext "error"
      hasErrorClass `shouldBe` False

    it "applies error style class if the command is unknown" $ do
      ctx <- loadDefaultGui
      let commandEntry = HC.commandEntry ctx
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
      ctx <- loadDefaultGui
      let notebook = HC.mainNotebook ctx
      editor <- launchNewEditorForText ctx notebook Nothing $ pack "initial text"
      buffer <- textViewGetBuffer editor
      sourceBufferUndo $ castToSourceBuffer buffer
      editorText <- getEditorText editor
      unpack editorText `shouldBe` "initial text"

    it "sets the tab title when opening a file" $ do
      ctx <- loadStubbedGui
      launchEditorTab ctx "/xxx/testName.hs"
      tabText <- getActiveEditorTabText ctx
      tabText `shouldBe` "testName.hs"

    it "updates the tab title to reflect if buffer is modified" $ do
      ctx <- launchNewFileAndSetModified
      tabText <- getActiveEditorTabText ctx
      tabText `shouldBe` "testName.hs*"

    it "focuses the tab with the open file if requested to open an already loaded file" $ do
      ctx <- loadStubbedGui
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
      (ctx, buffer) <- loadGuiAndPreviewSearch
      searchPreview ctx "!"
      tagStates <- checkSearchPreviewTagsAtRanges buffer [(0, 4), (15, 19)]
      tagStates `shouldBe` [(False, False), (False, False)]

    it "removes all search tags on reset" $ do
      (ctx, buffer) <- loadGuiAndPreviewSearch
      searchReset ctx
      tagStates <- checkSearchPreviewTagsAtRanges buffer [(0, 4), (15, 19)]
      tagStates `shouldBe` [(False, False), (False, False)]

    it "highlights the first match on execute" $ do
      (_, buffer) <- loadGuiAndExecuteSearch
      (start, end) <- getSelectionOffsets buffer
      (start, end) `shouldBe` (0, 4)

    it "highlights the next match from cursor on execute" $ do
      (ctx, buffer) <- loadGuiAndExecuteSearch
      searchExecute ctx "text"
      (start, end) <- getSelectionOffsets buffer
      (start, end) `shouldBe` (15, 19)

    it "wraps the search from start if there are no matches till the end on execute" $ do
      (ctx, buffer) <- loadGuiAndExecuteSearch
      searchExecute ctx "text"
      searchExecute ctx "text"
      (start, end) <- getSelectionOffsets buffer
      (start, end) `shouldBe` (0, 4)

    it "scrolls to the current match on execute" $ do
      ctx <- loadDefaultGui
      let notebook = HC.mainNotebook ctx
      let editorText = (concat . replicate 1000  $ "text - initial text! \n") ++ "customised search string at the end\n"
      editor <- launchNewEditorForText ctx notebook Nothing $ pack editorText
      processGtkEvents
      searchExecute ctx "customised search string at the end"
      processGtkEvents
      buffer <- textViewGetBuffer editor
      visible <- textViewGetVisibleRect editor
      caretIter <- textBufferGetIterAtMark buffer =<< textBufferGetInsert buffer
      cursor <- textViewGetIterLocation editor caretIter
      isRectangleInside visible cursor `shouldBe` True

  describe "editor commands" $ do
    it "closes the currently active editor tab" $ do
      ctx <- loadStubbedGui
      launchEditorTab ctx "/xxx/testName.hs"
      closeCurrentEditorTab ctx
      pagesAfterActivatingDirectory <- getNumberOfEditorPages ctx
      pagesAfterActivatingDirectory `shouldBe` 0

    it "saves the currently active file" $ do
      (mockedWriter, mockReader) <- mockedFileWriter
      sc <- HSC.defaultStyleContext "app-data"
      fc <- HFC.defaultFileContext stubbedFileLoader mockedWriter emptyFileTree
      ctx <- launchFileInContext fc sc "/xxx/testName.hs"
      saveCurrentEditorTab ctx emptyFileChooser
      savedFile <- mockReader
      savedFile `shouldBe` Just ("/xxx/testName.hs", pack "file contents for /xxx/testName.hs")

    it "skips save when there is no active file" $ do
      sc <- HSC.defaultStyleContext "app-data"
      fc <- HFC.defaultFileContext emptyFileLoader failingFileWriter emptyFileTree
      ctx <- loadGui fc sc
      saveCurrentEditorTab ctx emptyFileChooser
      
    it "marks buffer as unmodified on save" $ do
      sc <- HSC.defaultStyleContext "app-data"
      fc <- HFC.defaultFileContext stubbedFileLoader blackholeFileWriter emptyFileTree
      ctx <- launchFileInContext fc sc "/xxx/testName.hs"
      buffer <- textViewGetBuffer . fromJust =<< getActiveEditor ctx
      textBufferSetModified buffer True
      saveCurrentEditorTab ctx emptyFileChooser
      stateAfterSave <- textBufferGetModified buffer
      stateAfterSave `shouldBe` False

    it "creates a new unnamed file" $ do
      ctx <- launchNewFile
      pagesAfterActivatingDirectory <- getNumberOfEditorPages ctx
      pagesAfterActivatingDirectory `shouldBe` 1

    it "requests filename for a new file" $ do
      (mockedWriter, mockReader) <- mockedFileWriter
      sc <- HSC.defaultStyleContext "app-data"
      fc <- HFC.defaultFileContext emptyFileLoader mockedWriter emptyFileTree
      ctx <- launchNewFileInContextAndSaveAs fc sc "/xxx/fileResponded.hs"
      savedFile <- mockReader
      savedFile `shouldBe` Just ("/xxx/fileResponded.hs", pack "")
      tabText <- getActiveEditorTabText ctx
      tabText `shouldBe` "fileResponded.hs"

    it "updates the tab title from the newly got filepath" $ do
      sc <- HSC.defaultStyleContext "app-data"
      fc <- HFC.defaultFileContext emptyFileLoader blackholeFileWriter emptyFileTree
      ctx <- launchNewFileInContextAndSaveAs fc sc "/xxx/fileResponded.hs"
      buffer <- textViewGetBuffer . fromJust =<< getActiveEditor ctx
      textBufferSetModified buffer True
      tabText <- getActiveEditorTabText ctx
      tabText `shouldBe` "fileResponded.hs*"

launchNewFile :: IO HC.Context
launchNewFile = do
    ctx <- loadDefaultGui
    editNewFile ctx
    return ctx

launchFileInContext :: HFC.FileContext -> HSC.StyleContext -> String -> IO HC.Context
launchFileInContext fileCtx styleCtx filename = do
      ctx <- loadGui fileCtx styleCtx
      launchEditorTab ctx filename
      return ctx

launchNewFileInContextAndSaveAs :: HFC.FileContext -> HSC.StyleContext -> String -> IO HC.Context
launchNewFileInContextAndSaveAs fileCtx styleCtx filename = do
      ctx <- loadGui fileCtx styleCtx
      editNewFile ctx
      saveCurrentEditorTab ctx (stubbedFileChooser $ Just filename)
      return ctx

launchNewFileAndSetModified :: IO HC.Context
launchNewFileAndSetModified = do
    ctx <- loadStubbedGui
    launchEditorTab ctx "/xxx/testName.hs"
    buffer <- textViewGetBuffer . fromJust =<< getActiveEditor ctx
    textBufferSetModified buffer True
    return ctx

launchEditorTab :: HC.Context -> String -> IO ()
launchEditorTab ctx file = do
    let notebook = HC.mainNotebook ctx
    launchNewFileEditor ctx notebook file

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

getActiveEditorTabText :: HC.Context  -> IO String
getActiveEditorTabText ctx = do
    let notebook = HC.mainNotebook ctx
    currentlyActiveEditor <- getActiveEditorTab ctx
    text <- notebookGetTabLabelText notebook $ fromJust currentlyActiveEditor
    return $ fromJust text

getNumberOfEditorPages :: HC.Context -> IO Int
getNumberOfEditorPages = notebookGetNPages . HC.mainNotebook


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

blackholeFileWriter :: HFC.FileWriter
blackholeFileWriter _ _ = return ()

emptyFileTree :: HFC.FileTreeLoader
emptyFileTree = return []

emptyFileLoader :: HFC.FileLoader
emptyFileLoader _ = return $ Just $ pack ""

loadDefaultGui :: IO HC.Context
loadDefaultGui = do
    sc <- HSC.defaultStyleContext "app-data"
    fc <- HFC.defaultFileContext emptyFileLoader blackholeFileWriter emptyFileTree
    loadGui fc sc

loadStubbedGui :: IO HC.Context
loadStubbedGui = do
    sc <- HSC.defaultStyleContext "app-data"
    fc <- HFC.defaultFileContext stubbedFileLoader failingFileWriter fileTreeStub
    loadGui fc sc

loadDefaultGuiWithCommandAndItsStyleContext :: IO (HC.Context, Entry, StyleContext)
loadDefaultGuiWithCommandAndItsStyleContext = do
    ctx <- loadDefaultGui
    let commandEntry = HC.commandEntry ctx
    styleContext <- widgetGetStyleContext commandEntry
    return (ctx, commandEntry, styleContext)

loadGuiAndPreviewSearch :: IO (HC.Context, TextBuffer)
loadGuiAndPreviewSearch = do
    ctx <- loadDefaultGui
    let notebook = HC.mainNotebook ctx
    editor <- launchNewEditorForText ctx notebook Nothing $ pack "text - initial text!"
    searchPreview ctx "text"
    buffer <- textViewGetBuffer editor
    return (ctx, buffer)

loadGuiAndExecuteSearch :: IO (HC.Context, TextBuffer)
loadGuiAndExecuteSearch = do
    (ctx, buffer) <- loadGuiAndPreviewSearch
    iterBufferStart <- textBufferGetIterAtOffset buffer 0
    textBufferSelectRange buffer iterBufferStart iterBufferStart
    searchExecute ctx "text"
    return (ctx, buffer)

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
      where checkPair tag (l, r) = do
                iterL <- textBufferGetIterAtOffset buffer l
                iterR <- textBufferGetIterAtOffset buffer r
                checkL <- textIterBeginsTag iterL tag
                checkR <- textIterEndsTag iterR tag
                return (checkL, checkR)

toggleFocusOnCommandEntryAndReturnState :: HC.Context -> IO Bool
toggleFocusOnCommandEntryAndReturnState ctx = do
    toggleFocusOnCommandEntry ctx
    widgetGetIsFocus $ HC.commandEntry ctx

isRectangleInside :: Rectangle -> Rectangle -> Bool
isRectangleInside (Rectangle ax ay aw ah) (Rectangle bx by bw bh) =
    (ax <= bx) && (ay <= by) && (ax+aw >= bx+bw) && (ay+ah >= by+bh)

processGtkEvents :: IO ()
processGtkEvents = replicateM_ 500 $ mainIterationDo False
