module Hob.Command.FocusCommandEntrySpec (main, spec) where

import Data.Maybe
import Data.Text       (pack)
import Graphics.UI.Gtk

import qualified Hob.Context              as HC
import qualified Hob.Context.FileContext  as HFC
import qualified Hob.Context.StyleContext as HSC

import Hob.Command
import Hob.Command.FocusCommandEntry
import Hob.Command.NewTab
import Hob.Ui
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "focus command entry command" $ do
    it "focuses the command entry" $ do
      ctx <- loadDefaultGui
      focused <- toggleFocusOnCommandEntryAndReturnState ctx
      focused `shouldBe` True

    it "focus stays on toggle if there is no editor to focus to" $ do
      ctx <- loadDefaultGui
      commandExecute toggleFocusOnCommandEntryCommandHandler ctx
      focused <- toggleFocusOnCommandEntryAndReturnState ctx
      focused `shouldBe` True

    it "focus moves to editor on toggle" $ do
      ctx <- launchNewFile
      commandExecute toggleFocusOnCommandEntryCommandHandler ctx
      commandFocused <- toggleFocusOnCommandEntryAndReturnState ctx
      editorFocused <- widgetGetIsFocus . fromJust =<< getActiveEditor ctx
      commandFocused `shouldBe` False
      editorFocused `shouldBe` True

launchNewFile :: IO HC.Context
launchNewFile = do
    ctx <- loadDefaultGui
    editNewFile ctx
    return ctx

blackholeFileWriter :: HFC.FileWriter
blackholeFileWriter _ _ = return ()

emptyFileTree :: HFC.FileTreeLoader
emptyFileTree = return []

emptyFileLoader :: HFC.FileLoader
emptyFileLoader _ = return $ Just $ pack ""

loadDefaultGui :: IO HC.Context
loadDefaultGui = do
    sc <- HSC.defaultStyleContext "app-data"
    fc <- HFC.defaultFileContext emptyFileLoader blackholeFileWriter emptyFileTree
    loadGui fc sc

toggleFocusOnCommandEntryAndReturnState :: HC.Context -> IO Bool
toggleFocusOnCommandEntryAndReturnState ctx = do
    commandExecute toggleFocusOnCommandEntryCommandHandler ctx
    widgetGetIsFocus $ HC.commandEntry ctx
