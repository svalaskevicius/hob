module Hob.UiSpec (main, spec) where

import Data.Maybe
import Data.Text                            (unpack)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.General.StyleContext

import qualified Hob.Context as HC

import Hob.Ui
import Test.Hspec

import HobTest.Context.Default
import HobTest.Context.Stubbed

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "mainWindow" $
    it "is named" $ do
      ctx <- loadDefaultContext
      name <- widgetGetName $ HC.mainWindow ctx
      name `shouldBe` "mainWindow"

  describe "sidebar" $ do
    it "is named" $ do
      ctx <- loadDefaultContext
      name <- widgetGetName =<< getDirectoryListingSidebar ctx
      name `shouldBe` "directoryListing"

    it "opens a file editor" $ do
      ctx <- loadStubbedContext
      activateDirectoryPath ctx [1]
      editorText <- getActiveEditorText ctx
      (unpack . fromJust $ editorText) `shouldBe` "file contents for /xxx/c"

    it "does not open a file editor for directory" $ do
      ctx <- loadStubbedContext
      activateDirectoryPath ctx [0]
      pagesAfterActivatingDirectory <- getNumberOfEditorPages ctx
      pagesAfterActivatingDirectory `shouldBe` 0

    it "does not open a file editor for files it cannot read" $ do
      ctx <- loadStubbedContext
      activateDirectoryPath ctx [2]
      pagesAfterActivatingDirectory <- getNumberOfEditorPages ctx
      pagesAfterActivatingDirectory `shouldBe` 0

  describe "command entry" $ do
    it "is named" $ do
      ctx <- loadDefaultContext
      name <- widgetGetName $ HC.commandEntry ctx
      name `shouldBe` "commandEntry"

    it "initially there is no error class applied" $ do
      ctx <- loadDefaultContext
      styleContext <- widgetGetStyleContext $ HC.commandEntry ctx
      hasErrorClass <- styleContextHasClass styleContext "error"
      hasErrorClass `shouldBe` False

    it "applies error style class if the command is unknown" $ do
      ctx <- loadDefaultContext
      let commandEntry = HC.commandEntry ctx
      entrySetText commandEntry "qweqwe"
      styleContext <- widgetGetStyleContext commandEntry
      hasErrorClass <- styleContextHasClass styleContext "error"
      hasErrorClass `shouldBe` True

    it "removes error style on empty command" $ do
      (_, commandEntry, styleContext) <- loadDefaultGuiWithCommandAndItsStyleContext
      entrySetText commandEntry "not empty"
      styleContextAddClass styleContext "error"
      entrySetText commandEntry ""
      hasErrorClass <- styleContextHasClass styleContext "error"
      hasErrorClass `shouldBe` False

    it "removes error style on search command" $ do
      (_, commandEntry, styleContext) <- loadDefaultGuiWithCommandAndItsStyleContext
      styleContextAddClass styleContext "error"
      entrySetText commandEntry "/asd"
      hasErrorClass <- styleContextHasClass styleContext "error"
      hasErrorClass `shouldBe` False

activateDirectoryPath :: HC.Context -> TreePath -> IO ()
activateDirectoryPath ctx path = do
    treeView <- getDirectoryListingSidebar ctx
    firstColumn <- treeViewGetColumn treeView 0
    treeViewRowActivated treeView path $ fromJust firstColumn


getDirectoryListingSidebar :: HC.Context -> IO TreeView
getDirectoryListingSidebar ctx = do
    paned <- binGetChild $ HC.mainWindow ctx
    scrollbar <- panedGetChild1 $ castToPaned $ fromJust paned
    sidebar <- binGetChild $ castToScrolledWindow $ fromJust scrollbar
    return (castToTreeView $ fromJust sidebar)

getNumberOfEditorPages :: HC.Context -> IO Int
getNumberOfEditorPages = notebookGetNPages . HC.mainNotebook

loadDefaultGuiWithCommandAndItsStyleContext :: IO (HC.Context, Entry, StyleContext)
loadDefaultGuiWithCommandAndItsStyleContext = do
    ctx <- loadDefaultContext
    let commandEntry = HC.commandEntry ctx
    styleContext <- widgetGetStyleContext commandEntry
    return (ctx, commandEntry, styleContext)
