module HobTest.Editor (
    launchNewFile,
    launchEditorTab,
    getNumberOfEditorPages
    ) where

import Graphics.UI.Gtk

import           Hob.Command
import           Hob.Command.NewTab
import qualified Hob.Context           as HC
import qualified Hob.Context.UiContext as HC

import HobTest.Context.Default

launchNewFile :: IO HC.Context
launchNewFile = do
    ctx <- loadDefaultContext
    commandExecute editNewFileCommandHandler ctx
    return ctx

launchEditorTab :: HC.Context -> String -> IO ()
launchEditorTab ctx file = do
    let notebook = HC.mainNotebook . HC.uiContext $ ctx
    launchNewFileEditor ctx notebook file

getNumberOfEditorPages :: HC.Context -> IO Int
getNumberOfEditorPages = notebookGetNPages . HC.mainNotebook . HC.uiContext

