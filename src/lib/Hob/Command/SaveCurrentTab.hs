module Hob.Command.SaveCurrentTab (
        NewFileNameChooser,
        saveCurrentEditorTab,
        saveCurrentEditorTabHandler) where

import qualified Control.Monad.State        as S
import           Control.Monad.Trans        (liftIO)
import           Graphics.UI.Gtk
import           Graphics.UI.Gtk.SourceView (SourceView)

import Hob.Context
import Hob.Context.FileContext
import Hob.Context.UiContext
import Hob.Control
import Hob.Ui.Editor

type NewFileNameChooser = IO (Maybe FilePath)

saveCurrentEditorTab :: CommandHandler
saveCurrentEditorTab = CommandHandler Nothing (do
    ctx <- S.get
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
    ctx <- S.get
    maybeEditor <- liftIO $ getActiveEditor ctx
    maybeDo (saveInContext newFileNameChooser) maybeEditor


saveInContext :: NewFileNameChooser -> SourceView -> App ()
saveInContext newFileNameChooser editor = do
    ctx <- S.get
    liftIO $ saveEditor ctx
    where fileWriter ctx = contextFileWriter . fileContext $ ctx
          saveEditor ctx = do
              path <- getEditorFilePath editor
              case path of
                  Just filePath -> saveEditorContents ctx filePath
                  Nothing -> askForFile $ saveAsNewFile ctx
          askForFile onSuccess = newFileNameChooser >>= maybe (return()) onSuccess
          saveAsNewFile ctx filePath = do
              saveEditorContents ctx filePath
              setEditorFilePath editor $ Just filePath
              updateEditorTitle editor

          saveEditorContents ctx filePath = do
              textBuf <- textViewGetBuffer editor
              text <- get textBuf textBufferText
              fileWriter ctx filePath text
              textBuf `set` [textBufferModified := False]
              return ()
