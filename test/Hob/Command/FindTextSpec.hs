module Hob.Command.FindTextSpec (main, spec) where

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
    it "focuses editor on execute" $ do
      (ctx, _) <- loadGuiAndExecuteSearch
      editorFocused <- widgetGetIsFocus . fromJust =<< getActiveEditor ctx
      editorFocused `shouldBe` True

    it "enters the search mode" $ do
      (ctx, _) <- loadGuiAndExecuteSearch
      ref <- newIORef Nothing
      deferredRunner ctx $ activeModes >>= (liftIO . writeIORef ref)
      modes <- readIORef ref
      (modeName . last . fromJust) modes `shouldBe` "search"

  describe "search reset command on mode exit" $ do
    it "stops the next search" $ do
      (ctx, buffer) <- loadGuiAndExecuteSearch
      deferredRunner ctx exitLastMode
      deferredRunner ctx $ commandExecute searchNextCommandHandler
      (start, end) <- getSelectionOffsets buffer
      (start, end) `shouldBe` (0, 4)

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
