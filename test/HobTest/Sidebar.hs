module HobTest.Sidebar (
    sidebarTreeSearchErrorStateShouldBe,
    cursorShouldBeOnAfterSearch,
    cursorShouldBeOnAfterRefine,
    cursorShouldBeOnAfterSearchAndContinue,
    cursorShouldBeOn,
    ) where

import Data.Maybe
import Graphics.UI.Gtk
import Graphics.UI.Gtk.General.StyleContext

import qualified Hob.Context           as HC
import qualified Hob.Context.UiContext as HC
import           Hob.Ui.SidebarSearch

import Test.Hspec


sidebarTreeSearchErrorStateShouldBe :: HC.Context -> Bool -> IO ()
sidebarTreeSearchErrorStateShouldBe ctx state = do
      styleContext <- widgetGetStyleContext $ HC.sidebarTreeSearch . HC.uiContext $ ctx
      hasErrorClass <- styleContextHasClass styleContext "error"
      hasErrorClass `shouldBe` state

cursorShouldBeOnAfterSearch :: HC.Context -> String -> TreePath -> IO ()
cursorShouldBeOnAfterSearch ctx search expectedPath = do
      startSidebarSearch ctx search
      ctx `cursorShouldBeOn` expectedPath

cursorShouldBeOnAfterRefine :: HC.Context -> String -> TreePath -> IO ()
cursorShouldBeOnAfterRefine ctx search expectedPath = do
      entrySetText (HC.sidebarTreeSearch . HC.uiContext $ ctx) search
      ctx `cursorShouldBeOn` expectedPath

cursorShouldBeOnAfterSearchAndContinue :: HC.Context -> String -> [TreePath] -> IO ()
cursorShouldBeOnAfterSearchAndContinue ctx search expectedPaths = do
      startSidebarSearch ctx search
      ctx `cursorShouldBeOn` head expectedPaths
      mapM_ (\path -> do
              idx <- HC.runApp ctx initFileTreeIndex
              HC.runApp ctx $ continueSidebarSearch idx
              ctx `cursorShouldBeOn` path)
            $ tail expectedPaths
      mapM_ (\path -> do
              idx <- HC.runApp ctx initFileTreeIndex
              HC.runApp ctx $ continueSidebarSearchBackwards idx
              ctx `cursorShouldBeOn` path)
            $ reverse $ init expectedPaths

cursorShouldBeOn :: HC.Context -> TreePath -> IO ()
cursorShouldBeOn ctx expectedPath = do
    let sideBar = HC.sidebarTree . HC.uiContext $ ctx
    (path, column) <- treeViewGetCursor sideBar
    path `shouldBe` expectedPath
    isNothing column `shouldBe` True
