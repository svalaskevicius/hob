module Hob.Command.ReloadSidebarTreeSpec (main, spec) where

import Data.Maybe
import Data.Tree
import Graphics.UI.Gtk

import           Hob.Command.ReloadSidebarTree
import           Hob.Context
import qualified Hob.Context.FileContext       as HFC
import qualified Hob.Context.UiContext         as HC
import           Hob.DirectoryTree

import Test.Hspec

import HobTest.Context.Stubbed
import HobTest.Control
import HobTest.Sidebar

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "reload sidebar tree command" $ do
    it "loads new items in the tree" $ do
      ctx <- loadStubbedContext
      values <- getDirectoryListingSidebarRootItems ctx
      values `shouldBe` ["a", "a.b", "c", "-"]
      let ctx' = replaceStubbedTree ctx
      runCtxActions ctx' $ commandExecute reloadSidebarTreeCommandHandler
      values' <- getDirectoryListingSidebarRootItems ctx'
      values' `shouldBe` ["b","c"]

    it "reload sidebar search index" $ do
      ctx <- loadStubbedContext
      cursorShouldBeOnAfterSearch ctx "/new/" []
      let ctx' = replaceStubbedTree ctx
      runCtxActions ctx' $ commandExecute reloadSidebarTreeCommandHandler
      cursorShouldBeOnAfterSearch ctx "/new/" [1, 0]

replaceStubbedTree :: Context -> Context
replaceStubbedTree ctx = ctx{fileContext = newFileContext}
    where
        newFileContext = (fileContext ctx){HFC.contextFileTreeLoader = altFileTreeStub}

altFileTreeStub :: IO (Forest DirectoryTreeElement)
altFileTreeStub = return [
    Node (DirectoryTreeElement "b" "/xxx/b" False) [],
    Node (DirectoryTreeElement "c" "/xxx/c" True) [
        Node (DirectoryTreeElement "new" "/xxx/c/new" False) []
    ]]

getDirectoryListingSidebarRootItems :: Context -> IO [String]
getDirectoryListingSidebarRootItems ctx = do
    let treeView = HC.sidebarTree . uiContext $ ctx
    maybeTreeModel <- treeViewGetModel treeView
    let treeModel = fromJust maybeTreeModel
    maybeRoot <- treeModelGetIter treeModel [0]
    getSiblingItems treeModel maybeRoot

getSiblingItems :: TreeModel -> Maybe TreeIter -> IO [String]
getSiblingItems _ Nothing = return []
getSiblingItems treeModel (Just iter) = do
    let colId = makeColumnIdString 0
    value <- treeModelGetValue treeModel iter colId
    siblings <- getSiblingItems treeModel =<< treeModelIterNext treeModel iter
    return $ value : siblings


