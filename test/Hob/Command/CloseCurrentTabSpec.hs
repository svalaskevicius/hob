module Hob.Command.CloseCurrentTabSpec (main, spec) where

import Test.Hspec

import           Control.Monad.Error      (throwError)
import           Data.Text                (pack)
import           Data.Tree
import           Graphics.UI.Gtk
import qualified Hob.Context              as HC
import qualified Hob.Context.FileContext  as HFC
import qualified Hob.Context.StyleContext as HSC

import Hob.Command
import Hob.Command.CloseCurrentTab
import Hob.DirectoryTree
import Hob.Ui

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "close current tab command" $
        it "closes the currently active editor tab" $ do
            ctx <- loadStubbedGui
            launchEditorTab ctx "/xxx/testName.hs"
            commandExecute closeCurrentEditorTab ctx
            pagesAfterActivatingDirectory <- getNumberOfEditorPages ctx
            pagesAfterActivatingDirectory `shouldBe` 0

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

loadStubbedGui :: IO HC.Context
loadStubbedGui = do
    sc <- HSC.defaultStyleContext "app-data"
    fc <- HFC.defaultFileContext stubbedFileLoader failingFileWriter fileTreeStub
    loadGui fc sc

