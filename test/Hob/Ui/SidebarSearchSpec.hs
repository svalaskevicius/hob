module Hob.Ui.SidebarSearchSpec (main, spec) where

import Data.Maybe
import Data.Tree
import Graphics.UI.Gtk

import qualified Hob.Context              as HC
import qualified Hob.Context.FileContext  as HFC
import qualified Hob.Context.StyleContext as HSC
import           Hob.DirectoryTree
import           Hob.Ui
import           Hob.Ui.SidebarSearch

import Test.Hspec

import HobTest.Context.Default

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "sidebar search" $ do
    it "shows the named search box" $ do
      ctx <- sideBarSearchContext
      sideBar <- getDirectoryListingSidebar ctx
      searchEntry <- startSidebarSearch sideBar ""
      name <- widgetGetName searchEntry
      name `shouldBe` "sidebarSearchEntry"

    it "places the cursor on the first match on the search start" $ do
      ctx <- sideBarSearchContext
      sideBar <- getDirectoryListingSidebar ctx
      _ <- startSidebarSearch sideBar "greenFile"
      sideBar `cursorShouldBeOn` [1]

    it "places the cursor on the first nested match on the search start" $ do
      ctx <- sideBarSearchContext
      sideBar <- getDirectoryListingSidebar ctx
      _ <- startSidebarSearch sideBar "redFile"
      sideBar `cursorShouldBeOn` [0, 0]

    it "only looks for the leaf nodes" $ do
      ctx <- sideBarSearchContext
      sideBar <- getDirectoryListingSidebar ctx
      _ <- startSidebarSearch sideBar "red"
      sideBar `cursorShouldBeOn` [0, 0]

    it "uses fuzzy matching on file name" $ do
      ctx <- sideBarSearchContext
      sideBar <- getDirectoryListingSidebar ctx
      _ <- startSidebarSearch sideBar "rde"
      sideBar `cursorShouldBeOn` [0, 0]

    it "uses fuzzy matching on path" $ do
      ctx <- sideBarSearchContext
      sideBar <- getDirectoryListingSidebar ctx
      _ <- startSidebarSearch sideBar "rdDrrde"
      sideBar `cursorShouldBeOn` [0, 0]

    it "allows path separator while fuzzy matching on path" $ do
      ctx <- sideBarSearchContext
      sideBar <- getDirectoryListingSidebar ctx
      _ <- startSidebarSearch sideBar "r/rde"
      sideBar `cursorShouldBeOn` [0, 0]

cursorShouldBeOn :: TreeViewClass self => self -> TreePath -> IO ()
cursorShouldBeOn sideBar expectedPath = do
    (path, column) <- treeViewGetCursor sideBar
    path `shouldBe` expectedPath
    isNothing column `shouldBe` True

getDirectoryListingSidebar :: HC.Context -> IO TreeView
getDirectoryListingSidebar ctx = do
    paned <- binGetChild $ HC.mainWindow ctx
    scrollbar <- panedGetChild1 $ castToPaned $ fromJust paned
    sidebar <- binGetChild $ castToScrolledWindow $ fromJust scrollbar
    return (castToTreeView $ fromJust sidebar)

sideBarSearchFileTreeStub :: IO (Forest DirectoryTreeElement)
sideBarSearchFileTreeStub = return [
    Node (DirectoryTreeElement "redDir" "/xxx/redDir" True) [
        Node (DirectoryTreeElement "redFile" "/xxx/redDir/redFile" False) []],
    Node (DirectoryTreeElement "greenFile" "/xxx/greenFile" False) [],
    Node (DirectoryTreeElement "redFile" "/xxx/redFile" False) []]

sideBarSearchContext :: IO HC.Context
sideBarSearchContext = do
    sc <- HSC.defaultStyleContext "app-data"
    fc <- HFC.defaultFileContext emptyFileLoader blackholeFileWriter sideBarSearchFileTreeStub
    loadGui fc sc
