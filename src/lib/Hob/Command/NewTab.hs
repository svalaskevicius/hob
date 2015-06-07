module Hob.Command.NewTab (
            launchNewFileEditor,
            editNewFile,
            editNewFileCommandHandler,
            NewFileEditorLauncher) where


import           Control.Monad.Reader
import           Data.Text               (pack)
import           Graphics.UI.Gtk

import           Hob.Context
import           Hob.Context.FileContext
import           Hob.Context.UiContext
import           Hob.Control
import           Hob.Ui.Editor
import qualified Hob.Ui.Editor.Fancy     as Fancy

type NewFileEditorLauncher = FilePath -> IO ()

editNewFileCommandHandler :: CommandHandler
editNewFileCommandHandler = CommandHandler Nothing editNewFile

launchNewFileEditor :: Context -> Notebook -> NewFileEditorLauncher
launchNewFileEditor ctx targetNotebook filePath = do
    let fileLoader = contextFileLoader . fileContext $ ctx
    editorsForFile <- filterM isEditorFileMatching =<< (getEditors . editors $ ctx)
    case editorsForFile of
        [e] -> runApp ctx $ activateEditor e e targetNotebook
        _ -> maybeDo launchEditor =<< fileLoader filePath

    where launchEditor text = deferredRunner ctx $ launcherFunction targetNotebook (Just filePath) text
          isFancy = (reverse . take 6 . reverse $ filePath) == ".fancy"
          launcherFunction = if isFancy then Fancy.newEditorForText else newEditorForText
          isEditorFileMatching editor = do
              f <- runApp ctx $ getEditorFilePath editor editor
              return $ maybe False (filePath ==) f

editNewFile :: App()
editNewFile = do
    ctx <- ask
    let tabbed = mainNotebook.uiContext $ ctx
    newEditorForText tabbed Nothing $ pack ""

