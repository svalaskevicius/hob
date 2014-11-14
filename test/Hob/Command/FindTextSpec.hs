module Hob.Command.FindTextSpec (main, spec) where

import Control.Monad       (replicateM_)
import Control.Monad.State (liftIO)
import Data.IORef
import Data.Maybe
import Data.Text           (pack)
import Graphics.UI.Gtk
import Test.Hspec

import           Hob.Command.FindText
import           Hob.Context
import qualified Hob.Context.UiContext as HC
import           Hob.Ui                (getActiveEditor)
import           Hob.Ui.Editor         (newEditorForText)

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

    it "removes all search tags on preview reset" $ do
      (ctx, buffer) <- loadGuiAndPreviewSearch
      deferredRunner ctx $ (previewReset . fromJust . commandPreview) (searchCommandHandler "")
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
      deferredRunner ctx $ commandExecute (searchCommandHandler "text")
      (start, end) <- getSelectionOffsets buffer
      (start, end) `shouldBe` (15, 19)

    it "wraps the search from start if there are no matches till the end on execute" $ do
      (ctx, buffer) <- loadGuiAndExecuteSearch
      replicateM_ 3 $ deferredRunner ctx $ commandExecute (searchCommandHandler "text")
      (start, end) <- getSelectionOffsets buffer
      (start, end) `shouldBe` (0, 4)

    it "scrolls to the current match on execute" $
      ensureCursorVisibleAfterCommands $ commandExecute (searchCommandHandler "customised search string at the end")

    it "focuses editor on execute" $ do
      (ctx, _) <- loadGuiAndExecuteSearch
      editorFocused <- widgetGetIsFocus . fromJust =<< getActiveEditor ctx
      editorFocused `shouldBe` True

    it "enters the search mode" $ do
      (ctx, _) <- loadGuiAndExecuteSearch
      ref <- newIORef Nothing
      deferredRunner ctx $ activeModes >>= (\modes -> liftIO $ writeIORef ref modes)
      modes <- readIORef ref
      (modeName . last . fromJust) modes `shouldBe` "search"

  describe "search next command handler" $ do
    it "highlights the next match from cursor on execute" $ do
      (ctx, buffer) <- loadGuiAndExecuteSearch
      deferredRunner ctx $ commandExecute searchNextCommandHandler
      (start, end) <- getSelectionOffsets buffer
      (start, end) `shouldBe` (15, 19)

    it "wraps the search from start if there are no matches till the end on execute" $ do
      (ctx, buffer) <- loadGuiAndExecuteSearch
      replicateM_ 3 $ deferredRunner ctx $ commandExecute searchNextCommandHandler
      (start, end) <- getSelectionOffsets buffer
      (start, end) `shouldBe` (0, 4)

    it "scrolls to the current match on execute" $ do
      let commands = do
            commandExecute (searchCommandHandler "customised search string")
            commandExecute searchNextCommandHandler
      ensureCursorVisibleAfterCommands commands

  describe "search reset command on mode exit" $ do
    it "removes all search tags on cleanup" $ do
      (ctx, buffer) <- loadGuiAndExecuteSearch
      deferredRunner ctx $ exitLastMode
      tagStates <- checkSearchPreviewTagsAtRanges buffer [(0, 4), (15, 19)]
      tagStates `shouldBe` [(False, False), (False, False)]

    it "stops the next search" $ do
      (ctx, buffer) <- loadGuiAndExecuteSearch
      deferredRunner ctx $ exitLastMode
      deferredRunner ctx $ commandExecute searchNextCommandHandler
      (start, end) <- getSelectionOffsets buffer
      (start, end) `shouldBe` (0, 4)

  describe "search backwards command handler" $ do
    it "highlights the previous match from cursor on execute" $ do
      (ctx, buffer) <- loadGuiAndExecuteSearch
      deferredRunner ctx $ commandExecute searchNextCommandHandler
      deferredRunner ctx $ commandExecute searchBackwardsCommandHandler
      (start, end) <- getSelectionOffsets buffer
      (start, end) `shouldBe` (0, 4)

    it "wraps the search from start if there are no matches till the end on execute" $ do
      (ctx, buffer) <- loadGuiAndExecuteSearch
      deferredRunner ctx $ commandExecute searchBackwardsCommandHandler
      (start, end) <- getSelectionOffsets buffer
      (start, end) `shouldBe` (21, 25)

    it "scrolls to the current match on execute" $ do
      let commands = do
            commandExecute (searchCommandHandler "customised search string")
            commandExecute searchBackwardsCommandHandler
      ensureCursorVisibleAfterCommands commands

ensureCursorVisibleAfterCommands :: App() -> IO ()
ensureCursorVisibleAfterCommands commands = do
    ctx <- loadDefaultContext
    let notebook = HC.mainNotebook . uiContext $ ctx
    let editorText = (concat . replicate 1000  $ "text - initial text! \n") ++ "customised search string at the end\n"
    deferredRunner ctx $ newEditorForText notebook Nothing $ pack editorText
    mEditor <- getActiveEditor ctx
    let editor = fromJust mEditor
    processGtkEvents
    deferredRunner ctx commands
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
    mEditor <- getActiveEditor ctx
    let editor = fromJust mEditor
    deferredRunner ctx $ (previewExecute . fromJust . commandPreview) (searchCommandHandler "text")
    buffer <- textViewGetBuffer editor
    return (ctx, buffer)

loadGuiAndExecuteSearch :: IO (Context, TextBuffer)
loadGuiAndExecuteSearch = do
    (ctx, buffer) <- loadGuiAndPreviewSearch
    deferredRunner ctx $ (previewReset . fromJust . commandPreview) (searchCommandHandler "text")
    iterBufferStart <- textBufferGetIterAtOffset buffer 0
    textBufferSelectRange buffer iterBufferStart iterBufferStart
    deferredRunner ctx $ commandExecute (searchCommandHandler "text")
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

