module Hob.Command.SaveCurrentTab (
        NewFileNameChooser,
        saveCurrentEditorTab,
        saveCurrentEditorTabHandler) where

import Graphics.UI.Gtk

import Hob.Context
import Hob.Context.FileContext
import Hob.Context.UiContext
import Hob.Control
import Hob.Ui.Editor

type NewFileNameChooser = IO (Maybe FilePath)

saveCurrentEditorTab :: CommandHandler
saveCurrentEditorTab = CommandHandler Nothing (runWith saveCurrentEditorTabHandler fileChooser)
    where
        fileChooser ctx = do
            dialog <- fileChooserDialogNew Nothing (Just $ mainWindow.uiContext $ ctx) FileChooserActionSave [("Cancel", ResponseCancel), ("Save", ResponseOk)]
            resp <- dialogRun dialog
            file <- if resp == ResponseOk then fileChooserGetFilename dialog else return Nothing
            widgetDestroy dialog
            return file
        runWith a b ctx = a (b ctx) ctx

saveCurrentEditorTabHandler :: NewFileNameChooser -> Context -> IO ()
saveCurrentEditorTabHandler newFileNameChooser ctx =
    maybeDo saveEditor =<< getActiveEditor ctx
    where fileWriter = contextFileWriter . fileContext $ ctx
          saveEditor editor = do
              path <- getEditorFilePath editor
              case path of
                  Just filePath -> saveEditorContents editor filePath
                  Nothing -> askForFile $ saveAsNewFile editor
          askForFile onSuccess = newFileNameChooser >>= maybe (return()) onSuccess
          saveAsNewFile editor filePath = do
              saveEditorContents editor filePath
              setEditorFilePath editor $ Just filePath
              updateEditorTitle editor

          saveEditorContents editor filePath = do
              textBuf <- textViewGetBuffer editor
              text <- get textBuf textBufferText
              fileWriter filePath text
              textBuf `set` [textBufferModified := False]
              return ()
