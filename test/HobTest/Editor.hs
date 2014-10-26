module HobTest.Editor (
    launchNewFile,
    launchEditorTab,
    getNumberOfEditorPages
    ) where

import Graphics.UI.Gtk

import Hob.Command.NewTab
import Hob.Context
import Hob.Context.UiContext

import HobTest.Context.Default

launchNewFile :: IO Context
launchNewFile = do
    ctx <- loadDefaultContext
    commandExecute editNewFileCommandHandler ctx
    return ctx

launchEditorTab :: Context -> String -> IO ()
launchEditorTab ctx file = do
    let notebook = mainNotebook . uiContext $ ctx
    launchNewFileEditor ctx notebook file

getNumberOfEditorPages :: Context -> IO Int
getNumberOfEditorPages = notebookGetNPages . mainNotebook . uiContext

