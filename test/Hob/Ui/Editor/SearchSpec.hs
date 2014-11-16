module Hob.Ui.Editor.SearchSpec (main, spec) where

import Control.Monad       (replicateM_)
import Data.Maybe
import Data.Text           (pack)
import Graphics.UI.Gtk
import Test.Hspec
import           Graphics.UI.Gtk.SourceView (SourceView)

import           Hob.Context
import qualified Hob.Context.UiContext as HC
import           Hob.Ui.Editor         (newEditorForText, getActiveEditor)
import           Hob.Ui.Editor.Search

import HobTest.Context.Default

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "search for the first match" $ do
    it "creates search tag on preview" $ do
      (_, buffer) <- loadGuiAndPreviewSearch
      tagTable <- textBufferGetTagTable buffer
      tag <- textTagTableLookup tagTable "search"
      isNothing tag `shouldBe` False

    it "applies search tag for matches on preview" $ do
      (_, buffer) <- loadGuiAndPreviewSearch
      tagStates <- checkSearchPreviewTagsAtRanges buffer [(0, 4), (15, 19)]
      tagStates `shouldBe` [(True, True), (True, True)]

    it "removes all search tags on preview reset" $ do
      (ctx, buffer) <- loadGuiAndPreviewSearch
      Just editor <- getActiveEditor ctx
      resetSearchPreview editor
      tagStates <- checkSearchPreviewTagsAtRanges buffer [(0, 4), (15, 19)]
      tagStates `shouldBe` [(False, False), (False, False)]

    it "applies search tag for matches on execute" $ do
      (_, buffer) <- loadGuiAndExecuteSearch
      tagStates <- checkSearchPreviewTagsAtRanges buffer [(0, 4), (15, 19)]
      tagStates `shouldBe` [(True, True), (True, True)]

    it "highlights the first match on execute" $ do
      (_, buffer) <- loadGuiAndExecuteSearch
      (start, end) <- getSelectionOffsets buffer
      (start, end) `shouldBe` (0, 4)

    it "highlights the next match from cursor on execute" $ do
      (ctx, buffer) <- loadGuiAndExecuteSearch
      Just editor <- getActiveEditor ctx
      findFirstFromCursor editor "text"
      (start, end) <- getSelectionOffsets buffer
      (start, end) `shouldBe` (15, 19)

    it "wraps the search from start if there are no matches till the end on execute" $ do
      (ctx, buffer) <- loadGuiAndExecuteSearch
      Just editor <- getActiveEditor ctx
      replicateM_ 3 $ findFirstFromCursor editor "text"
      (start, end) <- getSelectionOffsets buffer
      (start, end) `shouldBe` (0, 4)

    it "scrolls to the current match on execute" $
      ensureCursorVisibleAfter $ \editor -> findFirstFromCursor editor "customised search string at the end"

  describe "search for the next match" $ do
    it "highlights the next match from cursor on execute" $ do
      (ctx, buffer) <- loadGuiAndExecuteSearch
      Just editor <- getActiveEditor ctx
      findNext editor
      (start, end) <- getSelectionOffsets buffer
      (start, end) `shouldBe` (15, 19)

    it "wraps the search from start if there are no matches till the end on execute" $ do
      (ctx, buffer) <- loadGuiAndExecuteSearch
      Just editor <- getActiveEditor ctx
      replicateM_ 3 $ findNext editor
      (start, end) <- getSelectionOffsets buffer
      (start, end) `shouldBe` (0, 4)

    it "scrolls to the current match on execute" $ do
      let commands editor = do
            findFirstFromCursor editor "customised search string"
            findNext editor
      ensureCursorVisibleAfter commands

  describe "search reset command on mode exit" $ do
    it "removes all search tags on cleanup" $ do
      (ctx, buffer) <- loadGuiAndExecuteSearch
      Just editor <- getActiveEditor ctx
      resetSearch editor
      tagStates <- checkSearchPreviewTagsAtRanges buffer [(0, 4), (15, 19)]
      tagStates `shouldBe` [(False, False), (False, False)]

    it "stops the next search" $ do
      (ctx, buffer) <- loadGuiAndExecuteSearch
      Just editor <- getActiveEditor ctx
      resetSearch editor
      findNext editor
      (start, end) <- getSelectionOffsets buffer
      (start, end) `shouldBe` (0, 4)

  describe "search for the previous match" $ do
    it "highlights the previous match from cursor on execute" $ do
      (ctx, buffer) <- loadGuiAndExecuteSearch
      Just editor <- getActiveEditor ctx
      findNext editor
      findPrevious editor
      (start, end) <- getSelectionOffsets buffer
      (start, end) `shouldBe` (0, 4)

    it "wraps the search from start if there are no matches till the end on execute" $ do
      (ctx, buffer) <- loadGuiAndExecuteSearch
      Just editor <- getActiveEditor ctx
      findPrevious editor
      (start, end) <- getSelectionOffsets buffer
      (start, end) `shouldBe` (21, 25)

    it "scrolls to the current match on execute" $ do
      let commands editor = do
            findFirstFromCursor editor "customised search string"
            findPrevious editor
      ensureCursorVisibleAfter commands

ensureCursorVisibleAfter :: (SourceView -> IO()) -> IO ()
ensureCursorVisibleAfter commands = do
    ctx <- loadDefaultContext
    let notebook = HC.mainNotebook . uiContext $ ctx
    let editorText = (concat . replicate 1000  $ "text - initial text! \n") ++ "customised search string at the end\n"
    deferredRunner ctx $ newEditorForText notebook Nothing $ pack editorText
    Just editor <- getActiveEditor ctx
    processGtkEvents
    commands editor
    processGtkEvents
    buffer <- textViewGetBuffer editor
    visible <- textViewGetVisibleRect editor
    caretIter <- textBufferGetIterAtMark buffer =<< textBufferGetInsert buffer
    cursor <- textViewGetIterLocation editor caretIter
    isRectangleInside visible cursor `shouldBe` True

loadGuiAndPreviewSearch :: IO (Context, TextBuffer)
loadGuiAndPreviewSearch = do
    ctx <- loadDefaultContext
    let notebook = HC.mainNotebook . uiContext $ ctx
    deferredRunner ctx $ newEditorForText notebook Nothing $ pack "text - initial text! text"
    Just editor <- getActiveEditor ctx
    highlightSearchPreview editor "text"
    buffer <- textViewGetBuffer editor
    return (ctx, buffer)

loadGuiAndExecuteSearch :: IO (Context, TextBuffer)
loadGuiAndExecuteSearch = do
    (ctx, buffer) <- loadGuiAndPreviewSearch
    Just editor <- getActiveEditor ctx
    resetSearchPreview editor
    iterBufferStart <- textBufferGetIterAtOffset buffer 0
    textBufferSelectRange buffer iterBufferStart iterBufferStart
    findFirstFromCursor editor "text"
    ctx' <- currentContext ctx
    return (ctx', buffer)

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

