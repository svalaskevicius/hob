module Hob.Command.CloseCurrentTabSpec (main, spec) where

import Test.Hspec

import Graphics.UI.Gtk

import           Hob.Command.CloseCurrentTab
import           Hob.Command.NewTab          (launchNewFileEditor)
import           Hob.Context
import qualified Hob.Context.UiContext       as HC

import HobTest.Context.Stubbed
import HobTest.Control

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "close current tab command" $
        it "closes the currently active editor tab" $ do
            ctx <- loadStubbedContext
            launchEditorTab ctx "/xxx/testName.hs"
            runCtxActions ctx $ commandExecute closeCurrentEditorTab
            pagesAfterActivatingDirectory <- getNumberOfEditorPages ctx
            pagesAfterActivatingDirectory `shouldBe` 0

launchEditorTab :: Context -> String -> IO ()
launchEditorTab ctx file = do
    let notebook = HC.mainNotebook . uiContext $ ctx
    launchNewFileEditor ctx notebook file

getNumberOfEditorPages :: Context -> IO Int
getNumberOfEditorPages = notebookGetNPages . HC.mainNotebook . uiContext

