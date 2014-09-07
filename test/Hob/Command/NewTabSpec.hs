module Hob.Command.NewTabSpec (main, spec) where

import Test.Hspec

import Control.Monad.Error (throwError)
import Data.Text           (pack)
import Data.Tree
import Graphics.UI.Gtk

import qualified Hob.Context              as HC
import qualified Hob.Context.FileContext  as HFC
import qualified Hob.Context.StyleContext as HSC

import Hob.Command
import Hob.Command.NewTab
import Hob.DirectoryTree
import Hob.Ui

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "new tab command" $ do
    it "creates a new unnamed file" $ do
      ctx <- launchNewFile
      pagesAfterActivatingDirectory <- getNumberOfEditorPages ctx
      pagesAfterActivatingDirectory `shouldBe` 1

    it "focuses the tab with the open file if requested to open an already loaded file" $ do
      ctx <- loadStubbedGui
      let notebook = HC.mainNotebook ctx
      launchEditorTab ctx "/xxx/testName.hs"
      currentPageOfFirstLoadedFile <- notebookGetCurrentPage notebook
      launchEditorTab ctx "/xxx/c"
      pagesBeforeOpeningExistingFile <- notebookGetNPages notebook
      launchEditorTab ctx "/xxx/testName.hs"
      currentPageAfterLoadingTheFirstLoadedFile <- notebookGetCurrentPage notebook
      pagesAfterOpeningExistingFile <- notebookGetNPages notebook
      pagesAfterOpeningExistingFile `shouldBe` pagesBeforeOpeningExistingFile
      currentPageAfterLoadingTheFirstLoadedFile `shouldBe` currentPageOfFirstLoadedFile


launchNewFile :: IO HC.Context
launchNewFile = do
    ctx <- loadDefaultGui
    commandExecute editNewFileCommandHandler ctx
    return ctx

launchEditorTab :: HC.Context -> String -> IO ()
launchEditorTab ctx file = do
    let notebook = HC.mainNotebook ctx
    launchNewFileEditor ctx notebook file

getNumberOfEditorPages :: HC.Context -> IO Int
getNumberOfEditorPages = notebookGetNPages . HC.mainNotebook


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
