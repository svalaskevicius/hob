module Hob.Command.FindTextSpec (main, spec) where

import Control.Monad (replicateM_)
import Test.Hspec

import Hob.Command
import Hob.Command.FindText
import Hob.Command.NewTab   (launchNewEditorForText)
import Hob.Ui               (loadGui)

import           Data.Maybe
import           Data.Text                (pack)
import           Graphics.UI.Gtk
import qualified Hob.Context              as HC
import qualified Hob.Context.FileContext  as HFC
import qualified Hob.Context.StyleContext as HSC

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "search command matcher" $ do
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
      commandExecute (searchCommandHandler "text") ctx
      commandExecute (searchCommandHandler "text") ctx
      (start, end) <- getSelectionOffsets buffer
      (start, end) `shouldBe` (0, 4)

    it "scrolls to the current match on execute" $ do
      ctx <- loadDefaultGui
      let notebook = HC.mainNotebook ctx
      let editorText = (concat . replicate 1000  $ "text - initial text! \n") ++ "customised search string at the end\n"
      editor <- launchNewEditorForText ctx notebook Nothing $ pack editorText
      processGtkEvents
      commandExecute (searchCommandHandler "customised search string at the end") ctx
      processGtkEvents
      buffer <- textViewGetBuffer editor
      visible <- textViewGetVisibleRect editor
      caretIter <- textBufferGetIterAtMark buffer =<< textBufferGetInsert buffer
      cursor <- textViewGetIterLocation editor caretIter
      isRectangleInside visible cursor `shouldBe` True


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

loadGuiAndPreviewSearch :: IO (HC.Context, TextBuffer)
loadGuiAndPreviewSearch = do
    ctx <- loadDefaultGui
    let notebook = HC.mainNotebook ctx
    editor <- launchNewEditorForText ctx notebook Nothing $ pack "text - initial text!"
    (previewExecute . fromJust . commandPreview) (searchCommandHandler "text") ctx
    buffer <- textViewGetBuffer editor
    return (ctx, buffer)

loadGuiAndExecuteSearch :: IO (HC.Context, TextBuffer)
loadGuiAndExecuteSearch = do
    (ctx, buffer) <- loadGuiAndPreviewSearch
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

