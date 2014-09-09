module Hob.Command.ReloadSidebarTreeSpec (main, spec) where

import Data.Maybe
import Data.Tree
import Graphics.UI.Gtk

import           Hob.Command
import           Hob.Command.ReloadSidebarTree
import qualified Hob.Context                   as HC
import qualified Hob.Context.FileContext       as HFC
--import           Hob.Ui
import Hob.DirectoryTree


import Test.Hspec

import HobTest.Context.Stubbed

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "reload sidebar tree command" $
    it "loads new items in the tree" $ do
      ctx <- loadStubbedContext
      values <- getDirectoryListingSidebarRootItems ctx
      values `shouldBe` ["a","c","-"]
      commandExecute reloadSidebarTreeCommandHandler $ replaceStubbedTree ctx
      values' <- getDirectoryListingSidebarRootItems ctx
      values' `shouldBe` ["b","c"]

replaceStubbedTree :: HC.Context -> HC.Context
replaceStubbedTree ctx = ctx{HC.fileContext = newFileContext}
    where
        newFileContext = (HC.fileContext ctx){HFC.contextFileTreeLoader = altFileTreeStub}

altFileTreeStub :: IO (Forest DirectoryTreeElement)
altFileTreeStub = return [
    Node (DirectoryTreeElement "b" "/xxx/b" True) [],
    Node (DirectoryTreeElement "c" "/xxx/c" False) []]

getDirectoryListingSidebarRootItems :: HC.Context -> IO [String]
getDirectoryListingSidebarRootItems ctx = do
    treeView <- getDirectoryListingSidebar ctx
    maybeTreeModel <- treeViewGetModel treeView
    let treeModel = fromJust maybeTreeModel
    maybeRoot <- treeModelGetIter treeModel [0]
    getSiblingItems treeModel maybeRoot

getDirectoryListingSidebar :: HC.Context -> IO TreeView
getDirectoryListingSidebar ctx = do
    paned <- binGetChild $ HC.mainWindow ctx
    scrollbar <- panedGetChild1 $ castToPaned $ fromJust paned
    sidebar <- binGetChild $ castToScrolledWindow $ fromJust scrollbar
    return (castToTreeView $ fromJust sidebar)

getSiblingItems :: TreeModel -> Maybe TreeIter -> IO [String]
getSiblingItems _ Nothing = return []
getSiblingItems treeModel (Just iter) = do
    let colId = makeColumnIdString 0
    value <- treeModelGetValue treeModel iter colId
    siblings <- getSiblingItems treeModel =<< treeModelIterNext treeModel iter
    return $ value : siblings


