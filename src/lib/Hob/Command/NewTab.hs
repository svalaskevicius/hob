module Hob.Command.NewTab (
            launchNewFileEditor,
            editNewFile,
            editNewFileCommandHandler,
            NewFileEditorLauncher) where


import           Control.Monad       (filterM, (<=<))
import qualified Control.Monad.State as S
import           Control.Monad.Trans (liftIO)
import           Data.Maybe          (mapMaybe)
import           Data.Text           (pack)
import           Graphics.UI.Gtk

import Hob.Context
import Hob.Context.FileContext
import Hob.Context.UiContext
import Hob.Control
import Hob.Ui.Editor

type NewFileEditorLauncher = FilePath -> IO ()

editNewFileCommandHandler :: CommandHandler
editNewFileCommandHandler = CommandHandler Nothing editNewFile

launchNewFileEditor :: Context -> Notebook -> NewFileEditorLauncher
launchNewFileEditor ctx targetNotebook filePath = do
    let fileLoader = contextFileLoader . fileContext $ ctx
    editors <- mapM getEditorFromNotebookTab <=< containerGetChildren $ targetNotebook
    editorsForFile <- filterM (\(_, ed) -> isEditorFileMatching ed ) $ numberedJusts editors
    case alreadyLoadedPage editorsForFile of
        Just nr -> notebookSetCurrentPage targetNotebook nr
        Nothing -> maybeDo launchEditor =<< fileLoader filePath

    where launchEditor text = do
              _ <- newEditorForText ctx targetNotebook (Just filePath) text
              return ()
          isEditorFileMatching editor = do
              f <- getEditorFilePath editor
              return $ maybe False (filePath ==) f
          alreadyLoadedPage [(nr, _)] = Just nr
          alreadyLoadedPage _ = Nothing

editNewFile :: Command
editNewFile = do
    ctx <- S.get
    let tabbed = mainNotebook.uiContext $ ctx
    _ <- liftIO $ newEditorForText ctx tabbed Nothing $ pack ""
    return ()

liftTupledMaybe :: (a, Maybe b) -> Maybe (a, b)
liftTupledMaybe (x, Just y) = Just (x, y)
liftTupledMaybe (_, Nothing) = Nothing

numberedJusts :: [Maybe a] -> [(Int, a)]
numberedJusts a = mapMaybe liftTupledMaybe $ zip [0..] a
