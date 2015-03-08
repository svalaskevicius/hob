module Hob.Command.SaveCurrentTab (
        NewFileNameChooser,
        saveCurrentEditorTab,
        saveCurrentEditorTabHandler) where

import           Control.Monad.Reader
import           Graphics.UI.Gtk

import           Hob.Context
import           Hob.Context.FileContext
import           Hob.Context.UiContext
import           Hob.Control
import           Hob.Context.Editor

type NewFileNameChooser = IO (Maybe FilePath)

saveCurrentEditorTab :: CommandHandler
saveCurrentEditorTab = CommandHandler Nothing (do
    ctx <- ask
    saveCurrentEditorTabHandler $ fileChooser ctx)
    where
        fileChooser :: Context -> IO (Maybe FilePath)
        fileChooser ctx = do
            dialog <- fileChooserDialogNew Nothing (Just $ mainWindow.uiContext $ ctx) FileChooserActionSave [("Cancel", ResponseCancel), ("Save", ResponseOk)]
            resp <- dialogRun dialog
            file <- if resp == ResponseOk then fileChooserGetFilename dialog else return Nothing
            widgetDestroy dialog
            return file

saveCurrentEditorTabHandler :: NewFileNameChooser -> App()
saveCurrentEditorTabHandler newFileNameChooser = do
    maybeEditor <- currentEditor
    maybeDo (saveInContext newFileNameChooser) maybeEditor


saveInContext :: NewFileNameChooser -> Editor -> App ()
saveInContext newFileNameChooser editor = do
    ctx <- ask
    saveEditor ctx
    where fileWriter ctx = contextFileWriter . fileContext $ ctx
          saveEditor ctx = do
              path <- getEditorFilePath editor editor
              case path of
                  Just filePath -> saveEditorContents ctx filePath
                  Nothing -> askForFile $ saveAsNewFile ctx
          askForFile onSuccess = liftIO newFileNameChooser >>= maybe (return()) onSuccess
          saveAsNewFile ctx filePath = do
              saveEditorContents ctx filePath
              updateEditor editor $ (\e -> setEditorFilePath e e $ Just filePath)
--              liftIO $ updateEditorTitle editor

          saveEditorContents ctx filePath = do
              text <- getEditorContents editor editor
              liftIO $ fileWriter ctx filePath text
--              textBuf `set` [textBufferModified := False]
              return ()
