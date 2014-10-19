module Hob.Command.FindTextSpec (main, spec) where

import Control.Monad (replicateM_)
import           Data.Maybe
import           Data.Text       (pack)
import           Graphics.UI.Gtk
import Test.Hspec

import           Hob.Command
import           Hob.Command.FindText
import qualified Hob.Context.UiContext as HC
import           Hob.Ui.Editor         (newEditorForText)
import qualified Hob.Context     as HC
import           Hob.Ui          (getActiveEditor)

import HobTest.Context.Default

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "search command handler" $ do
    it "creates search tag on preview" $ do
      (_, buffer) <- loadGuiAndPreviewSearch
      tagTable <- textBufferGetTagTable buffer
      tag <- textTagTableLookup tagTable "search"
      isNothing tag `shouldBe` False

    it "applies search tag for matches on preview" $ do
      (_, buffer) <- loadGuiAndPreviewSearch
      tagStates <- checkSearchPreviewTagsAtRanges buffer [(0, 4), (15, 19)]
      tagStates `shouldBe` [(True, True), (True, True)]

    it "removes all search tags on reset" $ do
      (ctx, buffer) <- loadGuiAndPreviewSearch
      (previewReset . fromJust . commandPreview) (searchCommandHandler "") ctx
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
      commandExecute (searchCommandHandler "text") ctx
      (start, end) <- getSelectionOffsets buffer
      (start, end) `shouldBe` (15, 19)

    it "wraps the search from start if there are no matches till the end on execute" $ do
      (ctx, buffer) <- loadGuiAndExecuteSearch
      replicateM_ 3 $ commandExecute (searchCommandHandler "text") ctx
      (start, end) <- getSelectionOffsets buffer
      (start, end) `shouldBe` (0, 4)

    it "scrolls to the current match on execute" $
      ensureCursorVisibleAfterCommands $ commandExecute (searchCommandHandler "customised search string at the end")

    it "focuses editor on execute" $ do
      (ctx, _) <- loadGuiAndExecuteSearch
      editorFocused <- widgetGetIsFocus . fromJust =<< getActiveEditor ctx
      editorFocused `shouldBe` True

  describe "search next command handler" $ do
    it "highlights the next match from cursor on execute" $ do
      (ctx, buffer) <- loadGuiAndExecuteSearch
      commandExecute searchNextCommandHandler ctx
      (start, end) <- getSelectionOffsets buffer
      (start, end) `shouldBe` (15, 19)

    it "wraps the search from start if there are no matches till the end on execute" $ do
      (ctx, buffer) <- loadGuiAndExecuteSearch
      replicateM_ 3 $ commandExecute searchNextCommandHandler ctx
      (start, end) <- getSelectionOffsets buffer
      (start, end) `shouldBe` (0, 4)

    it "scrolls to the current match on execute" $ do
      let commands ctx = do
            commandExecute (searchCommandHandler "customised search string") ctx
            commandExecute searchNextCommandHandler ctx
      ensureCursorVisibleAfterCommands commands

  describe "search reset command handler" $ do
    it "removes all search tags on reset" $ do
      (ctx, buffer) <- loadGuiAndPreviewSearch
      commandExecute searchResetCommandHandler ctx
      tagStates <- checkSearchPreviewTagsAtRanges buffer [(0, 4), (15, 19)]
      tagStates `shouldBe` [(False, False), (False, False)]

    it "stops the next search" $ do
      (ctx, buffer) <- loadGuiAndExecuteSearch
      commandExecute searchResetCommandHandler ctx
      commandExecute searchNextCommandHandler ctx
      (start, end) <- getSelectionOffsets buffer
      (start, end) `shouldBe` (0, 4)

  describe "search backwards command handler" $ do
    it "highlights the previous match from cursor on execute" $ do
      (ctx, buffer) <- loadGuiAndExecuteSearch
      commandExecute searchNextCommandHandler ctx
      commandExecute searchBackwardsCommandHandler ctx
      (start, end) <- getSelectionOffsets buffer
      (start, end) `shouldBe` (0, 4)

    it "wraps the search from start if there are no matches till the end on execute" $ do
      (ctx, buffer) <- loadGuiAndExecuteSearch
      commandExecute searchBackwardsCommandHandler ctx
      (start, end) <- getSelectionOffsets buffer
      (start, end) `shouldBe` (21, 25)

    it "scrolls to the current match on execute" $ do
      let commands ctx = do
            commandExecute (searchCommandHandler "customised search string") ctx
            commandExecute searchBackwardsCommandHandler ctx
      ensureCursorVisibleAfterCommands commands

ensureCursorVisibleAfterCommands :: (HC.Context -> IO ()) -> IO ()
ensureCursorVisibleAfterCommands commands = do
    ctx <- loadDefaultContext
    let notebook = HC.mainNotebook . HC.uiContext $ ctx
    let editorText = (concat . replicate 1000  $ "text - initial text! \n") ++ "customised search string at the end\n"
    editor <- newEditorForText ctx notebook Nothing $ pack editorText
    processGtkEvents
    commands ctx
    processGtkEvents
    buffer <- textViewGetBuffer editor
    visible <- textViewGetVisibleRect editor
    caretIter <- textBufferGetIterAtMark buffer =<< textBufferGetInsert buffer
    cursor <- textViewGetIterLocation editor caretIter
    isRectangleInside visible cursor `shouldBe` True

loadGuiAndPreviewSearch :: IO (HC.Context, TextBuffer)
loadGuiAndPreviewSearch = do
    ctx <- loadDefaultContext
    let notebook = HC.mainNotebook . HC.uiContext $ ctx
    editor <- newEditorForText ctx notebook Nothing $ pack "text - initial text! text"
    (previewExecute . fromJust . commandPreview) (searchCommandHandler "text") ctx
    buffer <- textViewGetBuffer editor
    return (ctx, buffer)

loadGuiAndExecuteSearch :: IO (HC.Context, TextBuffer)
loadGuiAndExecuteSearch = do
    (ctx, buffer) <- loadGuiAndPreviewSearch
    (previewReset . fromJust . commandPreview) (searchCommandHandler "text") ctx
    iterBufferStart <- textBufferGetIterAtOffset buffer 0
    textBufferSelectRange buffer iterBufferStart iterBufferStart
    commandExecute (searchCommandHandler "text") ctx
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

