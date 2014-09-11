module Hob.Ui.SidebarSearchSpec (main, spec) where

import Data.Maybe
import Graphics.UI.Gtk

import qualified Hob.Context as HC

import Hob.Ui.SidebarSearch

import Test.Hspec

import HobTest.Context.Default

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "sidebar search" $ do
    it "shows the named search box" $ do
      ctx <- loadDefaultContext
      sideBar <- getDirectoryListingSidebar ctx
      searchEntry <- startSidebarSearch sideBar ""
      name <- widgetGetName searchEntry
      name `shouldBe` "sidebarSearchEntry"

getDirectoryListingSidebar :: HC.Context -> IO TreeView
getDirectoryListingSidebar ctx = do
    paned <- binGetChild $ HC.mainWindow ctx
    scrollbar <- panedGetChild1 $ castToPaned $ fromJust paned
    sidebar <- binGetChild $ castToScrolledWindow $ fromJust scrollbar
    return (castToTreeView $ fromJust sidebar)

