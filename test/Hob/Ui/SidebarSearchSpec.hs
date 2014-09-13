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
      cursorShouldBeOnAfterSearch sideBar "greenFile" [1]

    it "places the cursor on the first nested match on the search start" $ do
      ctx <- sideBarSearchContext
      sideBar <- getDirectoryListingSidebar ctx
      cursorShouldBeOnAfterSearch sideBar "redFile" [0, 0]

    it "only looks for the leaf nodes" $ do
      ctx <- sideBarSearchContext
      sideBar <- getDirectoryListingSidebar ctx
      cursorShouldBeOnAfterSearch sideBar "red" [0, 0]

    it "uses fuzzy matching on file name" $ do
      ctx <- sideBarSearchContext
      sideBar <- getDirectoryListingSidebar ctx
      cursorShouldBeOnAfterSearch sideBar "rde" [0, 0]

    it "uses fuzzy matching on path" $ do
      ctx <- sideBarSearchContext
      sideBar <- getDirectoryListingSidebar ctx
      cursorShouldBeOnAfterSearch sideBar "rdDrrde" [0, 0]

    it "allows path separator while fuzzy matching on path" $ do
      ctx <- sideBarSearchContext
      sideBar <- getDirectoryListingSidebar ctx
      cursorShouldBeOnAfterSearch sideBar "r/rde" [0, 0]
      
    it "stays on the same path when search is refined" $ do
      ctx <- sideBarSearchContext
      sideBar <- getDirectoryListingSidebar ctx
      searchEntry <- startSidebarSearch sideBar "r/rd"
      sideBar `cursorShouldBeOn` [0, 0]
      entrySetText searchEntry "r/rde"
      updateSidebarSearch sideBar searchEntry
      sideBar `cursorShouldBeOn` [0, 0]

    it "moves to the new match when search is refined" $ do
      ctx <- sideBarSearchContext
      sideBar <- getDirectoryListingSidebar ctx
      searchEntry <- startSidebarSearch sideBar "r/rd"
      sideBar `cursorShouldBeOn` [0, 0]
      entrySetText searchEntry "r/rde2"
      updateSidebarSearch sideBar searchEntry
      sideBar `cursorShouldBeOn` [0, 1]

    it "focuses next file match when requested" $ do
      ctx <- sideBarSearchContext
      sideBar <- getDirectoryListingSidebar ctx
      cursorShouldBeOnAfterSearchAndContinue sideBar "rde" [[0, 0], [0, 1]]

    it "focuses next path match when requested" $ do
      ctx <- sideBarSearchContext
      sideBar <- getDirectoryListingSidebar ctx
      cursorShouldBeOnAfterSearchAndContinue sideBar "r/rde" [[0, 0], [0, 1]]

    it "focuses next multi-level path match when requested" $ do
      ctx <- sideBarSearchContext
      sideBar <- getDirectoryListingSidebar ctx
      cursorShouldBeOnAfterSearchAndContinue sideBar "/Dir//rde" [[3, 0, 0, 0], [4, 1, 0, 0]]

    it "finds next top-level file" $ do
      ctx <- sideBarSearchContext
      sideBar <- getDirectoryListingSidebar ctx
      cursorShouldBeOnAfterSearchAndContinue sideBar "greenFile" [[1], [3,0,0,0], [5]]

cursorShouldBeOnAfterSearch :: TreeView -> String -> TreePath -> IO ()
cursorShouldBeOnAfterSearch sideBar search expectedPath = do
      _ <- startSidebarSearch sideBar search
      sideBar `cursorShouldBeOn` expectedPath

cursorShouldBeOnAfterSearchAndContinue :: TreeView -> String -> [TreePath] -> IO ()
cursorShouldBeOnAfterSearchAndContinue sideBar search expectedPaths = do
      searchEntry <- startSidebarSearch sideBar search
      sideBar `cursorShouldBeOn` head expectedPaths
      mapM_ (\path -> do
              continueSidebarSearch sideBar searchEntry
              sideBar `cursorShouldBeOn` path)
            $ tail expectedPaths
      mapM_ (\path -> do
              continueSidebarSearchBackwards sideBar searchEntry
              sideBar `cursorShouldBeOn` path)
            $ reverse $ init expectedPaths

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
        Node (DirectoryTreeElement "redFile1" "/xxx/redDir/redFile1" False) [],
        Node (DirectoryTreeElement "redFile2" "/xxx/redDir/redFile2" False) []],
    Node (DirectoryTreeElement "greenFile" "/xxx/greenFile" False) [],
    Node (DirectoryTreeElement "redFile" "/xxx/redFile" False) [],
    Node (DirectoryTreeElement "greenDir" "/xxx/greenDir" True) [
        Node (DirectoryTreeElement "greenDir" "/xxx/greenDir/greenDir" True) [
            Node (DirectoryTreeElement "greenDir" "/xxx/greenDir/greenDir/greenDir" True) [
                Node (DirectoryTreeElement "redFile" "/xxx/greenDir/greenDir/greenDir/redFile" False) []]]],
    Node (DirectoryTreeElement "blueFolder" "/xxx/blueFolder" True) [
        Node (DirectoryTreeElement "blueFolder" "/xxx/blueFolder/blueFolder" True) [
            Node (DirectoryTreeElement "blueFolder" "/xxx/blueFolder/blueFolder/blueFolder" True) [
                Node (DirectoryTreeElement "redFile" "/xxx/blueFolder/blueFolder/blueFolder/redFile" False) []]],
        Node (DirectoryTreeElement "blueDir" "/xxx/blueDir/blueDir" True) [
            Node (DirectoryTreeElement "blueDir" "/xxx/blueDir/blueDir/blueDir" True) [
                Node (DirectoryTreeElement "redFile" "/xxx/blueDir/blueDir/blueDir/redFile" False) []]]],
    Node (DirectoryTreeElement "greenFile" "/xxx/greenFile" False) []]

sideBarSearchContext :: IO HC.Context
sideBarSearchContext = do
    sc <- HSC.defaultStyleContext "app-data"
    fc <- HFC.defaultFileContext emptyFileLoader blackholeFileWriter sideBarSearchFileTreeStub
    loadGui fc sc
