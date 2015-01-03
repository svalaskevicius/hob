module Hob.Command.NewTab (
            launchNewFileEditor,
            editNewFile,
            editNewFileCommandHandler,
            NewFileEditorLauncher) where


import Control.Monad.Reader
import Data.Maybe           (mapMaybe)
import Data.Text            (pack)
import Graphics.UI.Gtk

import Hob.Context
import Hob.Context.FileContext
import Hob.Context.UiContext
import Hob.Control
import Hob.Ui.Editor
import qualified Hob.Ui.Editor.Fancy as Fancy

type NewFileEditorLauncher = FilePath -> IO ()

editNewFileCommandHandler :: CommandHandler
editNewFileCommandHandler = CommandHandler Nothing editNewFile

launchNewFileEditor :: Context -> Notebook -> NewFileEditorLauncher
launchNewFileEditor ctx targetNotebook filePath = do
    let fileLoader = contextFileLoader . fileContext $ ctx
    currentEditors <- mapM getEditorFromNotebookTab <=< containerGetChildren $ targetNotebook
    editorsForFile <- filterM (\(_, ed) -> isEditorFileMatching ed ) $ numberedJusts currentEditors
    case alreadyLoadedPage editorsForFile of
        Just nr -> notebookSetCurrentPage targetNotebook nr
        Nothing -> maybeDo launchEditor =<< fileLoader filePath

    where launchEditor text = deferredRunner ctx $ launcherFunction targetNotebook (Just filePath) text
          isFancy = (reverse . take 4 . reverse $ filePath) == ".txt"
          launcherFunction = if isFancy then Fancy.newEditorForText else newEditorForText
          isEditorFileMatching editor = do
              f <- getEditorFilePath editor
              return $ maybe False (filePath ==) f
          alreadyLoadedPage [(nr, _)] = Just nr
          alreadyLoadedPage _ = Nothing

editNewFile :: App()
editNewFile = do
    ctx <- ask
    let tabbed = mainNotebook.uiContext $ ctx
    newEditorForText tabbed Nothing $ pack ""

liftTupledMaybe :: (a, Maybe b) -> Maybe (a, b)
liftTupledMaybe (x, Just y) = Just (x, y)
liftTupledMaybe (_, Nothing) = Nothing

numberedJusts :: [Maybe a] -> [(Int, a)]
numberedJusts a = mapMaybe liftTupledMaybe $ zip [0..] a
