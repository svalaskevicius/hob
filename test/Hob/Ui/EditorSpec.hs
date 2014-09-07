module Hob.Ui.EditorSpec (main, spec) where

import Test.Hspec

import Control.Monad.Error        (throwError)
import Data.Maybe
import Data.Text                  (pack, unpack)
import Data.Tree
import Graphics.UI.Gtk
import Graphics.UI.Gtk.SourceView (SourceView, castToSourceBuffer,
                                   sourceBufferUndo)

import qualified Hob.Context              as HC
import qualified Hob.Context.FileContext  as HFC
import qualified Hob.Context.StyleContext as HSC

import Hob.DirectoryTree
import Hob.Ui

import Hob.Ui.Editor

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "editor ui" $ do
    it "does not allow to undo the intial loaded source" $ do
      ctx <- loadDefaultGui
      editor <- launchEditorTab ctx $ Just "/xxx/testName.hs"
      buffer <- textViewGetBuffer editor
      sourceBufferUndo $ castToSourceBuffer buffer
      editorText <- getEditorText editor
      unpack editorText `shouldBe` "initial text"

    it "sets the tab title when opening a file" $ do
      ctx <- loadStubbedGui
      _ <- launchEditorTab ctx $ Just "/xxx/testName.hs"
      tabText <- getActiveEditorTabText ctx
      tabText `shouldBe` "testName.hs"

    it "updates the tab title to reflect if buffer is modified" $ do
      ctx <- launchNewFileAndSetModified
      tabText <- getActiveEditorTabText ctx
      tabText `shouldBe` "testName.hs*"

    it "retrieves active editor" $ do
      ctx <- loadDefaultGui
      editor <- launchEditorTab ctx $ Just "/xxx/testName.hs"
      activeEditor <- getActiveEditor ctx
      fromJust activeEditor == editor `shouldBe` True

    it "retrieves editor text" $ do
      ctx <- loadDefaultGui
      editor <- launchEditorTab ctx $ Just "/xxx/testName.hs"
      text <- getEditorText editor
      text `shouldBe` pack "initial text"

    it "retrieves active editor text" $ do
      ctx <- loadDefaultGui
      _ <- launchEditorTab ctx $ Just "/xxx/testName.hs"
      text <- getActiveEditorText ctx
      fromJust text `shouldBe` pack "initial text"

    it "retrieves Nothing for active editor text if there is no active editor" $ do
      ctx <- loadDefaultGui
      text <- getActiveEditorText ctx
      text `shouldBe` Nothing

    it "retrieves current editor from notebook" $ do
      ctx <- loadDefaultGui
      editor <- launchEditorTab ctx $ Just "/xxx/testName.hs"
      tab <- getActiveEditorTab ctx
      activeEditor <- getEditorFromNotebookTab $ fromJust tab
      fromJust activeEditor == editor `shouldBe` True

    it "retrieves the set filepath for an editor" $ do
      ctx <- loadDefaultGui
      editor <- launchEditorTab ctx $ Just "/xxx/testName.hs"
      filePath1 <- getEditorFilePath editor
      setEditorFilePath editor $ Just "/tmp/xxx"
      filePath2 <- getEditorFilePath editor
      filePath1 `shouldBe` Just "/xxx/testName.hs"
      filePath2 `shouldBe` Just "/tmp/xxx"

launchNewFileAndSetModified :: IO HC.Context
launchNewFileAndSetModified = do
    ctx <- loadStubbedGui
    _ <- launchEditorTab ctx $ Just "/xxx/testName.hs"
    buffer <- textViewGetBuffer . fromJust =<< getActiveEditor ctx
    textBufferSetModified buffer True
    return ctx

launchEditorTab :: HC.Context -> Maybe FilePath -> IO SourceView
launchEditorTab ctx file = do
    let notebook = HC.mainNotebook ctx
    newEditorForText ctx notebook file $ pack "initial text"

getActiveEditorTabText :: HC.Context  -> IO String
getActiveEditorTabText ctx = do
    let notebook = HC.mainNotebook ctx
    currentlyActiveEditor <- getActiveEditorTab ctx
    text <- notebookGetTabLabelText notebook $ fromJust currentlyActiveEditor
    return $ fromJust text


fileTreeStub :: IO (Forest DirectoryTreeElement)
fileTreeStub = return [
    Node (DirectoryTreeElement "a" "/xxx/a" True) [
        Node (DirectoryTreeElement "b" "/xxx/a/b" False) []],
    Node (DirectoryTreeElement "c" "/xxx/c" False) [],
    Node (DirectoryTreeElement "-" "/xxx/cannotRead" False) []]

failingFileWriter :: HFC.FileWriter
failingFileWriter _ _ = throwError $ userError "cannot write files stub"

stubbedFileLoader :: HFC.FileLoader
stubbedFileLoader "/xxx/c" = return $ Just $ pack "file contents for /xxx/c"
stubbedFileLoader "/xxx/cannotRead" = return Nothing
stubbedFileLoader "/xxx/testName.hs" = return $ Just $ pack "file contents for /xxx/testName.hs"
stubbedFileLoader path = throwError $ userError $ "cannot open unknown file: "++path

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
