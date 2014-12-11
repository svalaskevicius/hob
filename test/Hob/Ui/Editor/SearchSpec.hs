module Hob.Ui.Editor.SearchSpec (main, spec) where

import Control.Monad              (replicateM_)
import Data.Maybe
import Data.Text                  (pack)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.SourceView (SourceView)
import Test.Hspec

import           Hob.Context
import qualified Hob.Context.UiContext as HC
import           Hob.Ui.Editor         (getActiveEditor, newEditorForText)
import           Hob.Ui.Editor.Search

import HobTest.Context.Default
import HobTest.Control

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describeSearchPreview loadGuiAndPreviewSearch resetSearchPreview
  describeSearchExecute loadGuiAndExecuteSearch findFirstFromCursor
  describeSearchExecute loadGuiAndInitReplace (\editor text -> startReplace editor text "-")

  describe "search for the next match" $ do
    it "highlights the next match from cursor on execute" $ do
      buffer <- loadEditorAndExecuteAfterSearch findNext
      buffer `shouldHaveSelectionOffsets` (15, 19)

    it "wraps the search from start if there are no matches till the end on execute" $ do
      buffer <- loadEditorAndExecuteAfterSearch $ \editor -> replicateM_ 3 $ findNext editor
      buffer `shouldHaveSelectionOffsets` (0, 4)

    it "scrolls to the current match on execute" $ do
      let commands editor = do
            findFirstFromCursor editor "customised search string"
            findNext editor
      ensureCursorVisibleAfter commands

  describe "search reset command on mode exit" $ do
    it "removes all search tags on cleanup" $ do
      buffer <- loadEditorAndExecuteAfterSearch resetSearch
      tagStates <- checkSearchPreviewTagsAtRanges buffer [(0, 4), (15, 19)]
      tagStates `shouldBe` [(False, False), (False, False)]

    it "stops the next search" $ do
      buffer <- loadEditorAndExecuteAfterSearch $ \editor -> resetSearch editor >> findNext editor
      buffer `shouldHaveSelectionOffsets` (0, 4)

  describe "search for the previous match" $ do
    it "highlights the previous match from cursor on execute" $ do
      buffer <- loadEditorAndExecuteAfterSearch $ \editor -> findNext editor >> findPrevious editor
      buffer `shouldHaveSelectionOffsets` (0, 4)

    it "wraps the search from start if there are no matches till the end on execute" $ do
      buffer <- loadEditorAndExecuteAfterSearch findPrevious
      buffer `shouldHaveSelectionOffsets` (21, 25)

    it "scrolls to the current match on execute" $ do
      let commands editor = do
            findFirstFromCursor editor "customised search string"
            findPrevious editor
      ensureCursorVisibleAfter commands

  describe "replace next" $ do
    it "does not replace if there is no highlighted text" $ do
      (ctx, buffer) <- loadGuiWithEditor
      withActiveEditor ctx replaceNext
      buffer `shouldHaveText` "text - initial text! text"

    it "replaces previously highlighted text" $ do
      (ctx, buffer) <- loadGuiAndInitReplace
      withActiveEditor ctx replaceNext
      buffer `shouldHaveText` "New:TEXT:New - initial text! text"

    it "does not replace if the highlighted text doesnt match the search string" $ do
      (ctx, buffer) <- loadGuiAndInitReplace
      (s, _) <- textBufferGetSelectionBounds buffer
      e <- textBufferGetIterAtOffset buffer 7
      textBufferSelectRange buffer s e
      withActiveEditor ctx replaceNext
      buffer `shouldHaveText` "text - initial text! text"

    it "stops the next search" $ do
      (ctx, buffer) <- loadGuiAndInitReplace
      withActiveEditor ctx $ \editor -> do
          resetReplace editor
          findNext editor
      buffer `shouldHaveSelectionOffsets` (0, 4)

    it "stops the next replace" $ do
      (ctx, buffer) <- loadGuiAndInitReplace
      withActiveEditor ctx $ \editor -> do
          resetReplace editor
          findFirstFromCursor editor "text"
          replaceNext editor
      buffer `shouldHaveText` "text - initial text! text"

describeSearchPreview :: IO (Context, TextBuffer) -> (SourceView -> IO ()) -> Spec
describeSearchPreview initializePreview resetFnc =
  describe "search preview for the first match" $ do
    it "creates search tag on preview" $ do
      (_, buffer) <- initializePreview
      tagTable <- textBufferGetTagTable buffer
      tag <- textTagTableLookup tagTable "search"
      isNothing tag `shouldBe` False

    it "applies search tag for matches on preview" $ do
      (_, buffer) <- initializePreview
      tagStates <- checkSearchPreviewTagsAtRanges buffer [(0, 4), (15, 19)]
      tagStates `shouldBe` [(True, True), (True, True)]

    it "removes all search tags on preview reset" $ do
      (ctx, buffer) <- initializePreview
      withActiveEditor ctx resetFnc
      tagStates <- checkSearchPreviewTagsAtRanges buffer [(0, 4), (15, 19)]
      tagStates `shouldBe` [(False, False), (False, False)]

describeSearchExecute :: IO (Context, TextBuffer) -> (SourceView -> String -> IO ()) -> Spec
describeSearchExecute initializeExecute invokeFirstFnc =
  describe "search execute for the first match" $ do
    it "applies search tag for matches on execute" $ do
      (_, buffer) <- initializeExecute
      tagStates <- checkSearchPreviewTagsAtRanges buffer [(0, 4), (15, 19)]
      tagStates `shouldBe` [(True, True), (True, True)]

    it "highlights the first match on execute" $ do
      (_, buffer) <- initializeExecute
      buffer `shouldHaveSelectionOffsets` (0, 4)

    it "highlights the next match from cursor on execute" $ do
      (ctx, buffer) <- initializeExecute
      withActiveEditor ctx (`invokeFirstFnc` "text")
      buffer `shouldHaveSelectionOffsets` (15, 19)

    it "wraps the search from start if there are no matches till the end on execute" $ do
      (ctx, buffer) <- initializeExecute
      withActiveEditor ctx $ \editor -> replicateM_ 3 $ invokeFirstFnc editor "text"
      buffer `shouldHaveSelectionOffsets` (0, 4)

    it "scrolls to the current match on execute" $
      ensureCursorVisibleAfter $ \editor -> invokeFirstFnc editor "customised search string at the end"

shouldHaveSelectionOffsets :: TextBuffer -> (Int, Int) -> IO ()
shouldHaveSelectionOffsets buffer offsets = do
    (start, end) <- getSelectionOffsets buffer
    (start, end) `shouldBe` offsets

shouldHaveText :: TextBuffer -> String -> IO ()
shouldHaveText buffer text = do
    bufferText <- (buffer `get` textBufferText)::IO String
    bufferText `shouldBe` text

loadEditorAndExecuteAfterSearch :: (SourceView -> IO ()) -> IO TextBuffer
loadEditorAndExecuteAfterSearch actions = do
    (ctx, buffer) <- loadGuiAndExecuteSearch
    withActiveEditor ctx actions
    return buffer


ensureCursorVisibleAfter :: (SourceView -> IO()) -> IO ()
ensureCursorVisibleAfter commands = do
    ctx <- loadDefaultContext
    let notebook = HC.mainNotebook . uiContext $ ctx
    let editorText = (concat . replicate 1000  $ "text - initial text! \n") ++ "customised search string at the end\n"
    runCtxActions ctx $ newEditorForText notebook Nothing $ pack editorText
    processGtkEvents
    withActiveEditor ctx commands
    processGtkEvents
    withActiveEditor ctx $ \editor -> do
        buffer <- textViewGetBuffer editor
        visible <- textViewGetVisibleRect editor
        caretIter <- textBufferGetIterAtMark buffer =<< textBufferGetInsert buffer
        cursor <- textViewGetIterLocation editor caretIter
        isRectangleInside visible cursor `shouldBe` True

loadGuiAndPreviewSearch :: IO (Context, TextBuffer)
loadGuiAndPreviewSearch = do
    ctx <- loadDefaultContext
    let notebook = HC.mainNotebook . uiContext $ ctx
    runCtxActions ctx $ newEditorForText notebook Nothing $ pack "text - initial text! text"
    Just editor <- getActiveEditor ctx
    highlightSearchPreview editor "text"
    buffer <- textViewGetBuffer editor
    return (ctx, buffer)

loadGuiAndExecuteSearch :: IO (Context, TextBuffer)
loadGuiAndExecuteSearch = loadGuiAndExecute $ \editor -> findFirstFromCursor editor "text"

loadGuiAndInitReplace:: IO (Context, TextBuffer)
loadGuiAndInitReplace = loadGuiAndExecute $ \editor -> startReplace editor "text" "New:TEXT:New"

loadGuiWithEditor :: IO (Context, TextBuffer)
loadGuiWithEditor = loadGuiAndExecute $ const $ return ()

loadGuiAndExecute :: (SourceView -> IO()) -> IO (Context, TextBuffer)
loadGuiAndExecute actions = do
    (ctx, buffer) <- loadGuiAndPreviewSearch
    withActiveEditor ctx resetSearchPreview
    iterBufferStart <- textBufferGetIterAtOffset buffer 0
    textBufferSelectRange buffer iterBufferStart iterBufferStart
    withActiveEditor ctx actions
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

isRectangleInside :: Rectangle -> Rectangle -> Bool
isRectangleInside (Rectangle ax ay aw ah) (Rectangle bx by bw bh) =
    (ax <= bx) && (ay <= by) && (ax+aw >= bx+bw) && (ay+ah >= by+bh)

processGtkEvents :: IO ()
processGtkEvents = replicateM_ 500 $ mainIterationDo False

withActiveEditor :: Context -> (SourceView -> IO b) -> IO b
withActiveEditor ctx actions = do
    Just editor <- getActiveEditor ctx
    actions editor

