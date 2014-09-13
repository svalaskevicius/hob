module Hob.Command.CloseCurrentTabSpec (main, spec) where

import Test.Hspec

import Graphics.UI.Gtk

import           Hob.Command
import           Hob.Command.CloseCurrentTab
import           Hob.Command.NewTab          (launchNewFileEditor)
import qualified Hob.Context                 as HC
import qualified Hob.Context.UiContext       as HC

import HobTest.Context.Stubbed

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "close current tab command" $
        it "closes the currently active editor tab" $ do
            ctx <- loadStubbedContext
            launchEditorTab ctx "/xxx/testName.hs"
            commandExecute closeCurrentEditorTab ctx
            pagesAfterActivatingDirectory <- getNumberOfEditorPages ctx
            pagesAfterActivatingDirectory `shouldBe` 0

launchEditorTab :: HC.Context -> String -> IO ()
launchEditorTab ctx file = do
    let notebook = HC.mainNotebook . HC.uiContext $ ctx
    launchNewFileEditor ctx notebook file

getNumberOfEditorPages :: HC.Context -> IO Int
getNumberOfEditorPages = notebookGetNPages . HC.mainNotebook . HC.uiContext

