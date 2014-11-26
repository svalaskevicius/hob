module Hob.Context.Editor (
    initEditors,
    runOnEditor,
    enterMode,
    exitLastMode,
    activeModes,
    getActiveCommands,
    ) where

import Control.Concurrent.MVar
import Control.Monad.Reader
import Data.Monoid

import Hob.Context.Events
import Hob.Context.Types


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
    updateActiveEditor $ \editor -> runOnEditor enterEditorMode editor mode
    emitEvent $ Event "core.mode.change"

activeModes :: App (Maybe [Mode])
activeModes = do
    active <- currentEditor
    maybe (return Nothing) (runOnEditor modeStack >=> return . Just) active

exitLastMode :: App()
exitLastMode = do
    updateActiveEditor $ \editor -> runOnEditor exitLastEditorMode editor
    emitEvent $ Event "core.mode.change"

updateActiveEditor :: (Editor -> App Editor) -> App()
updateActiveEditor actions = do
    ctx <- ask
    editorList <- fromContext editors
    liftIO $ updateEditors editorList $ \oldEditors -> runReaderT (updateActiveEditorHandler oldEditors) ctx
    where
        updateActiveEditorHandler oldEditors = do
            (e1, e2) <- splitBeforeFirstActive oldEditors
            if null e2 then return oldEditors
            else do
                let active = head e2
                active' <- actions active
                return $ e1 ++ [active'] ++ tail e2
        splitBeforeFirstActive [] = return ([], [])
        splitBeforeFirstActive (x:xs) = do
            active <- runOnEditor isCurrentlyActive x
            if active then return ([], x:xs)
            else do
                (n, ns) <- splitBeforeFirstActive xs
                return (x:n, ns)

getActiveCommands :: App CommandMatcher
getActiveCommands = do
    base <- asks baseCommands
    maybeModes <- activeModes
    maybe (return base)
          (\modes -> return $ base `mappend` mconcat (fmap commandMatcher modes))
          maybeModes
