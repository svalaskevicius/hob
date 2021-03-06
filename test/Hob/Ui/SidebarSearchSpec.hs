module Hob.Ui.SidebarSearchSpec (main, spec) where

import Data.Tree
import Graphics.UI.Gtk

import qualified Hob.Context              as HC
import qualified Hob.Context.FileContext  as HFC
import qualified Hob.Context.StyleContext as HSC
import qualified Hob.Context.UiContext    as HC
import           Hob.DirectoryTree
import           Hob.Ui
import           Hob.Ui.SidebarSearch

import Test.Hspec

import HobTest.Context.Default
import HobTest.Sidebar

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "sidebar search" $ do
    it "shows the named search box" $ do
      ctx <- sideBarSearchContext
      startSidebarSearch ctx ""
      name <- widgetGetName $ HC.sidebarTreeSearch . HC.uiContext $ ctx
      name `shouldBe` "sidebarSearchEntry"

    it "places the cursor on the first match on the search start" $ do
      ctx <- sideBarSearchContext
      cursorShouldBeOnAfterSearch ctx "greenFile" [1]

    it "finds part even if start can be eaten" $ do
      ctx <- sideBarSearchContext
      cursorShouldBeOnAfterSearch ctx "greenFileZ" [6]

    it "places the cursor on the first nested match on the search start" $ do
      ctx <- sideBarSearchContext
      cursorShouldBeOnAfterSearch ctx "redFile" [0, 0]

    it "places the cursor on the first match on the search start even if started from the middle" $ do
      ctx <- sideBarSearchContext
      treeViewSetCursor (HC.sidebarTree . HC.uiContext $ ctx) [1] Nothing
      cursorShouldBeOnAfterSearch ctx "redFile" [0, 0]

    it "places the cursor on the first match again on the search start even if had the same search text before" $ do
      ctx <- sideBarSearchContext
      cursorShouldBeOnAfterSearch ctx "redFile" [0, 0]
      treeViewSetCursor (HC.sidebarTree . HC.uiContext $ ctx) [1] Nothing
      cursorShouldBeOnAfterSearch ctx "redFile" [0, 0]

    it "only looks for the leaf nodes" $ do
      ctx <- sideBarSearchContext
      cursorShouldBeOnAfterSearch ctx "red" [0, 0]

    it "uses fuzzy matching on file name" $ do
      ctx <- sideBarSearchContext
      cursorShouldBeOnAfterSearch ctx "r d e" [0, 0]

    it "uses fuzzy matching on path" $ do
      ctx <- sideBarSearchContext
      cursorShouldBeOnAfterSearch ctx "r dD r r d e" [0, 0]

    it "allows path separator while fuzzy matching on path" $ do
      ctx <- sideBarSearchContext
      cursorShouldBeOnAfterSearch ctx "r/red e" [0, 0]

    it "stays on the same path when search is refined" $ do
      ctx <- sideBarSearchContext
      cursorShouldBeOnAfterSearch ctx "r/r d" [0, 0]
      cursorShouldBeOnAfterRefine ctx "r/r d e" [0, 0]

    it "moves to the new match when search is refined" $ do
      ctx <- sideBarSearchContext
      cursorShouldBeOnAfterSearch ctx "r/r d" [0, 0]
      cursorShouldBeOnAfterRefine ctx "r/r d e2" [0, 1]

    it "focuses next file match when requested" $ do
      ctx <- sideBarSearchContext
      cursorShouldBeOnAfterSearchAndContinue ctx "r d e" [[0, 0], [0, 1]]

    it "focuses next path match when requested" $ do
      ctx <- sideBarSearchContext
      cursorShouldBeOnAfterSearchAndContinue ctx "r/r d e" [[0, 0], [0, 1]]

    it "focuses next multi-level path match when requested" $ do
      ctx <- sideBarSearchContext
      cursorShouldBeOnAfterSearchAndContinue ctx "/ Dir//r d e" [[3, 0, 0, 0], [4, 1, 0, 0]]

    it "collapses previously matched path after searching in next subtree" $ do
      ctx <- sideBarSearchContext
      startSidebarSearch ctx "/Dir//r d e"
      idx <- HC.runApp ctx initFileTreeIndex
      HC.runApp ctx $ continueSidebarSearch idx
      expanded <- treeViewRowExpanded (HC.sidebarTree . HC.uiContext $ ctx) [3]
      expanded `shouldBe` False

    it "allows case insensitive search" $ do
      ctx <- sideBarSearchContext
      cursorShouldBeOnAfterSearchAndContinue ctx "/ DIR//R D E" [[3, 0, 0, 0], [4, 1, 0, 0]]

    it "finds next top-level file" $ do
      ctx <- sideBarSearchContext
      cursorShouldBeOnAfterSearchAndContinue ctx "green file" [[1], [3,0,0,0], [5]]

    it "applies error style class if there are no matches" $ do
      ctx <- sideBarSearchContext
      startSidebarSearch ctx "AAA!!!AAA"
      ctx `sidebarTreeSearchErrorStateShouldBe` True

    it "removes error style on empty search text" $ do
      ctx <- sideBarSearchContext
      startSidebarSearch ctx "AAA!!!AAA"
      entrySetText (HC.sidebarTreeSearch . HC.uiContext $ ctx) ""
      ctx `sidebarTreeSearchErrorStateShouldBe` False

    it "removes error style on a match" $ do
      ctx <- sideBarSearchContext
      startSidebarSearch ctx "AAA!!!AAA"
      entrySetText (HC.sidebarTreeSearch . HC.uiContext $ ctx) "red"
      ctx `sidebarTreeSearchErrorStateShouldBe` False

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
        Node (DirectoryTreeElement "blueDir" "/xxx/blueFolder/blueDir" True) [
            Node (DirectoryTreeElement "blueDir" "/xxx/blueFolder/blueDir/blueDir" True) [
                Node (DirectoryTreeElement "redFile" "/xxx/blueFolder/blueDir/blueDir/redFile" False) []]]],
    Node (DirectoryTreeElement "greenFile2" "/xxx/greenFile2" False) [],
    Node (DirectoryTreeElement "greegreenFileZ" "/xxx/greegreenFileZ" False) []]

sideBarSearchContext :: IO HC.Context
sideBarSearchContext = do
    sc <- HSC.defaultStyleContext "app-data"
    fc <- HFC.defaultFileContext emptyFileLoader blackholeFileWriter sideBarSearchFileTreeStub
    loadGui fc sc
