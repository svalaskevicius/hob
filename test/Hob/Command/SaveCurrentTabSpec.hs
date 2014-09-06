module Hob.Command.SaveCurrentTabSpec (main, spec) where

import Control.Monad.Error (throwError)
import Data.IORef
import Data.Maybe
import Data.Text           (Text, pack)
import Graphics.UI.Gtk

import qualified Hob.Context              as HC
import qualified Hob.Context.FileContext  as HFC
import qualified Hob.Context.StyleContext as HSC

import Hob.Command.SaveCurrentTab
import Hob.Ui
import Test.Hspec

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
      saveCurrentEditorTabHandler emptyFileChooser ctx
      savedFile <- mockReader
      savedFile `shouldBe` Just ("/xxx/testName.hs", pack "file contents for /xxx/testName.hs")

    it "skips save when there is no active file" $ do
      sc <- HSC.defaultStyleContext "app-data"
      fc <- HFC.defaultFileContext emptyFileLoader failingFileWriter emptyFileTree
      ctx <- loadGui fc sc
      saveCurrentEditorTabHandler emptyFileChooser ctx

    it "marks buffer as unmodified on save" $ do
      sc <- HSC.defaultStyleContext "app-data"
      fc <- HFC.defaultFileContext stubbedFileLoader blackholeFileWriter emptyFileTree
      ctx <- launchFileInContext fc sc "/xxx/testName.hs"
      buffer <- textViewGetBuffer . fromJust =<< getActiveEditor ctx
      textBufferSetModified buffer True
      saveCurrentEditorTabHandler emptyFileChooser ctx
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
      editNewFile ctx
      saveCurrentEditorTabHandler (stubbedFileChooser $ Just filename) ctx
      return ctx

launchEditorTab :: HC.Context -> String -> IO ()
launchEditorTab ctx file = do
    let notebook = HC.mainNotebook ctx
    launchNewFileEditor ctx notebook file

getActiveEditorTabText :: HC.Context  -> IO String
getActiveEditorTabText ctx = do
    let notebook = HC.mainNotebook ctx
    currentlyActiveEditor <- getActiveEditorTab ctx
    text <- notebookGetTabLabelText notebook $ fromJust currentlyActiveEditor
    return $ fromJust text

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
