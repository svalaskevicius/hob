module Hob.Ui.EditorSpec (main, spec) where

import Test.Hspec

import qualified Control.Monad.State        as S
import           Data.IORef
import           Data.Maybe
import           Data.Monoid
import           Data.Text                  (pack, unpack)
import           Graphics.UI.Gtk
import           Graphics.UI.Gtk.SourceView (SourceView, castToSourceBuffer,
                                             sourceBufferUndo)

import           Hob.Context
import qualified Hob.Context.UiContext as HC
import           Hob.Ui
import           Hob.Ui.Editor

import HobTest.Context.Default

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "editor ui" $ do
    it "does not allow to undo the intial loaded source" $ do
      ctx <- loadDefaultContext
      editor <- launchEditorTab ctx $ Just "/xxx/testName.hs"
      buffer <- textViewGetBuffer editor
      sourceBufferUndo $ castToSourceBuffer buffer
      editorText <- getEditorText editor
      unpack editorText `shouldBe` "initial text"

    it "sets the tab title when opening a file" $ do
      ctx <- loadDefaultContext
      _ <- launchEditorTab ctx $ Just "/xxx/testName.hs"
      tabText <- getActiveEditorTabText ctx
      tabText `shouldBe` "testName.hs"

    it "updates the tab title to reflect if buffer is modified" $ do
      ctx <- launchNewFileAndSetModified
      tabText <- getActiveEditorTabText ctx
      tabText `shouldBe` "testName.hs*"

    it "retrieves active editor" $ do
      ctx <- loadDefaultContext
      editor <- launchEditorTab ctx $ Just "/xxx/testName.hs"
      activeEditor <- getActiveEditor ctx
      fromJust activeEditor == editor `shouldBe` True

    it "registers the new editor in the context on creation" $ do
      ctx <- loadDefaultContext
      _ <- launchEditorTab ctx $ Just "/xxx/testName.hs"
      retRef <- newIORef Nothing
      deferredRunner ctx $ S.get >>= (\ctx' -> S.liftIO $ writeIORef retRef (Just . editors $ ctx'))
      registeredEditors <- readIORef retRef
      (length . fromJust) registeredEditors `shouldBe` 1

    it "retrieves editor text" $ do
      ctx <- loadDefaultContext
      editor <- launchEditorTab ctx $ Just "/xxx/testName.hs"
      text <- getEditorText editor
      text `shouldBe` pack "initial text"

    it "retrieves active editor text" $ do
      ctx <- loadDefaultContext
      _ <- launchEditorTab ctx $ Just "/xxx/testName.hs"
      text <- getActiveEditorText ctx
      fromJust text `shouldBe` pack "initial text"

    it "retrieves Nothing for active editor text if there is no active editor" $ do
      ctx <- loadDefaultContext
      text <- getActiveEditorText ctx
      text `shouldBe` Nothing

    it "retrieves current editor from notebook" $ do
      ctx <- loadDefaultContext
      editor <- launchEditorTab ctx $ Just "/xxx/testName.hs"
      tab <- getActiveEditorTab ctx
      activeEditor <- getEditorFromNotebookTab $ fromJust tab
      fromJust activeEditor == editor `shouldBe` True

    it "retrieves the set filepath for an editor" $ do
      ctx <- loadDefaultContext
      editor <- launchEditorTab ctx $ Just "/xxx/testName.hs"
      filePath1 <- getEditorFilePath editor
      setEditorFilePath editor $ Just "/tmp/xxx"
      filePath2 <- getEditorFilePath editor
      filePath1 `shouldBe` Just "/xxx/testName.hs"
      filePath2 `shouldBe` Just "/tmp/xxx"


    it "invokes mode teardown on exitMode" $ do
      record <- newIORef False
      ctx <- loadDefaultContext
      editor <- launchEditorTab ctx $ Just "/xxx/testName.hs"
      widgetGrabFocus editor
      deferredRunner ctx $ enterMode $ Mode "testmode" mempty (S.liftIO $ writeIORef record True)
      deferredRunner ctx exitLastMode
      cleanupInvoked <- readIORef record
      cleanupInvoked `shouldBe` True

launchNewFileAndSetModified :: IO Context
launchNewFileAndSetModified = do
    ctx <- loadDefaultContext
    _ <- launchEditorTab ctx $ Just "/xxx/testName.hs"
    buffer <- textViewGetBuffer . fromJust =<< getActiveEditor ctx
    textBufferSetModified buffer True
    return ctx

launchEditorTab :: Context -> Maybe FilePath -> IO SourceView
launchEditorTab ctx file = do
    let notebook = HC.mainNotebook . uiContext $ ctx
    deferredRunner ctx $ newEditorForText notebook file $ pack "initial text"
    mEditor <- getActiveEditor ctx
    return $ fromJust mEditor

getActiveEditorTabText :: Context  -> IO String
getActiveEditorTabText ctx = do
    let notebook = HC.mainNotebook . uiContext $ ctx
    currentlyActiveEditor <- getActiveEditorTab ctx
    text <- notebookGetTabLabelText notebook $ fromJust currentlyActiveEditor
    return $ fromJust text
