module Hob.Command.SaveCurrentTabSpec (main, spec) where

import Data.IORef
import Data.Maybe
import Data.Text       (Text, pack)
import Graphics.UI.Gtk

import           Hob.Command.NewTab
import           Hob.Command.SaveCurrentTab
import qualified Hob.Context                as HC
import qualified Hob.Context.FileContext    as HFC
import qualified Hob.Context.StyleContext   as HSC
import qualified Hob.Context.UiContext      as HC
import           Hob.Ui

import Test.Hspec

import HobTest.Context.Default
import HobTest.Context.Stubbed

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "save current tab command" $ do
    it "saves the currently active file" $ do
      (mockedWriter, mockReader) <- mockedFileWriter
      sc <- HSC.defaultStyleContext "app-data"
      fc <- HFC.defaultFileContext stubbedFileLoader mockedWriter emptyFileTree
      ctx <- launchFileInContext fc sc "/xxx/testName.hs"
      _ <- saveCurrentEditorTabHandler emptyFileChooser ctx
      savedFile <- mockReader
      savedFile `shouldBe` Just ("/xxx/testName.hs", pack "file contents for /xxx/testName.hs")

    it "skips save when there is no active file" $ do
      sc <- HSC.defaultStyleContext "app-data"
      fc <- HFC.defaultFileContext emptyFileLoader failingFileWriter emptyFileTree
      ctx <- loadGui fc sc
      _ <- saveCurrentEditorTabHandler emptyFileChooser ctx
      return ()

    it "marks buffer as unmodified on save" $ do
      sc <- HSC.defaultStyleContext "app-data"
      fc <- HFC.defaultFileContext stubbedFileLoader blackholeFileWriter emptyFileTree
      ctx <- launchFileInContext fc sc "/xxx/testName.hs"
      buffer <- textViewGetBuffer . fromJust =<< getActiveEditor ctx
      textBufferSetModified buffer True
      _ <- saveCurrentEditorTabHandler emptyFileChooser ctx
      stateAfterSave <- textBufferGetModified buffer
      stateAfterSave `shouldBe` False

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

launchFileInContext :: HFC.FileContext -> HSC.StyleContext -> String -> IO HC.Context
launchFileInContext fileCtx styleCtx filename = do
    ctx <- loadGui fileCtx styleCtx
    launchEditorTab ctx filename
    return ctx

launchNewFileInContextAndSaveAs :: HFC.FileContext -> HSC.StyleContext -> String -> IO HC.Context
launchNewFileInContextAndSaveAs fileCtx styleCtx filename = do
    ctx <- loadGui fileCtx styleCtx
    _ <- editNewFile ctx
    _ <- saveCurrentEditorTabHandler (stubbedFileChooser $ Just filename) ctx
    return ctx

launchEditorTab :: HC.Context -> String -> IO ()
launchEditorTab ctx file = do
    let notebook = HC.mainNotebook . HC.uiContext $ ctx
    launchNewFileEditor ctx notebook file

getActiveEditorTabText :: HC.Context  -> IO String
getActiveEditorTabText ctx = do
    let notebook = HC.mainNotebook . HC.uiContext $ ctx
    currentlyActiveEditor <- getActiveEditorTab ctx
    text <- notebookGetTabLabelText notebook $ fromJust currentlyActiveEditor
    return $ fromJust text

mockedFileWriter :: IO (HFC.FileWriter, IO (Maybe (String, Text)))
mockedFileWriter = do
    recorder <- newIORef Nothing
    return (curry $ writeIORef recorder . Just, readIORef recorder)

stubbedFileChooser :: Maybe FilePath -> NewFileNameChooser
stubbedFileChooser = return

emptyFileChooser :: NewFileNameChooser
emptyFileChooser = stubbedFileChooser Nothing
