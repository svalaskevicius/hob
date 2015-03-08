module Hob.Context.Editor (
    initEditors,
    runOnEditor,
    updateEditor,
    enterMode,
    exitLastMode,
    activeModes,
    getActiveCommands,
    currentEditor
    ) where

import           Control.Concurrent.MVar
import           Control.Monad.Reader
import           Data.Monoid

import           Hob.Context.Events
import           Hob.Context.Types
import Hob.Control


runOnEditor :: (Editor -> Editor -> a) -> Editor -> a
runOnEditor f editor = f editor editor

initEditors :: IO EditorList
initEditors = do
    editorList <- newMVar []
    return $ EditorList (updateEditorsHandler editorList) (getEditorsHandler editorList)
    where
        updateEditorsHandler editorList updater = do
            oldEditors <- takeMVar editorList
            newEditors <- updater oldEditors
            putMVar editorList newEditors
        getEditorsHandler = readMVar

currentEditor :: App (Maybe Editor)
currentEditor = do
    editorList <- fromContext editors
    active <- filterM (\e -> isCurrentlyActive e e) =<< liftIO (getEditors editorList)
    return $
        if null active then Nothing
        else Just $ head active

enterMode :: Mode -> App()
enterMode mode = do
    updateActiveEditor $ \editor -> do
        editor' <- clearEditorModes editor
        runOnEditor enterEditorMode editor' mode
    emitNamedEvent "core.mode.change"
    where clearEditorModes editor = do
              modes <- runOnEditor modeStack editor
              if not . null $ modes then do
                  editor' <- runOnEditor exitLastEditorMode editor
                  clearEditorModes editor'
              else return editor

activeModes :: App (Maybe [Mode])
activeModes = do
    active <- currentEditor
    maybe (return Nothing) (runOnEditor modeStack >=> return . Just) active

exitLastMode :: App()
exitLastMode = do
    updateActiveEditor $ \editor -> runOnEditor exitLastEditorMode editor
    emitNamedEvent "core.mode.change"

updateActiveEditor :: (Editor -> App Editor) -> App()
updateActiveEditor actions = do
    current <- currentEditor
    maybeDo (\e -> updateEditor e actions) current

updateEditor :: Editor -> (Editor -> App Editor) -> App()
updateEditor editor actions = do
    ctx <- ask
    editorList <- fromContext editors
    liftIO $ updateEditors editorList $ \oldEditors -> runReaderT (updateEditorHandler oldEditors) ctx
    where
        updateEditorHandler oldEditors = do
            eid <- editorId editor editor
            (e1, e2) <- splitBeforeEditorId eid oldEditors
            if null e2 then return oldEditors
            else do
                editor' <- actions editor
                return $ e1 ++ [editor'] ++ drop 1 e2
        splitBeforeEditorId _ [] = return ([], [])
        splitBeforeEditorId eid (x:xs) = do
            eid' <- editorId x x
            if eid == eid' then return ([], x:xs)
            else do
                (n, ns) <- splitBeforeEditorId eid xs
                return (x:n, ns)

getActiveCommands :: App CommandMatcher
getActiveCommands = do
    base <- asks baseCommands
    maybeModes <- activeModes
    maybe (return base)
          (\modes -> return $ base `mappend` mconcat (fmap commandMatcher modes))
          maybeModes
