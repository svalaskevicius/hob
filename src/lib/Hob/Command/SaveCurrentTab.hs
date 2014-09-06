module Hob.Command.SaveCurrentTab (
        NewFileNameChooser,
        saveCurrentEditorTab,
        saveCurrentEditorTabHandler) where


import Graphics.UI.Gtk

import Hob.Command
import Hob.Context


import Control.Monad              ((<=<))
import Filesystem.Path.CurrentOS  (decodeString, encodeString, filename)
import Graphics.UI.Gtk.SourceView (SourceView, castToSourceView)

import Hob.Context.FileContext
import System.Glib.GObject

type NewFileNameChooser = IO (Maybe FilePath)

saveCurrentEditorTab :: CommandHandler
saveCurrentEditorTab = CommandHandler Nothing (runWith saveCurrentEditorTabHandler fileChooser)
    where
        fileChooser ctx = do
            dialog <- fileChooserDialogNew Nothing (Just $ mainWindow ctx) FileChooserActionSave [("Cancel", ResponseCancel), ("Save", ResponseOk)]
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
              quark <- fileNameQuark
              path <- objectGetAttributeUnsafe quark editor
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


fileNameQuark :: IO Quark
fileNameQuark = quarkFromString "fileName"

getActiveEditor :: Context -> IO (Maybe SourceView)
getActiveEditor = maybe (return Nothing) getEditorFromNotebookTab <=< getActiveEditorTab

getEditorFromNotebookTab :: Widget -> IO (Maybe SourceView)
getEditorFromNotebookTab currentlyActiveEditor =
    if currentlyActiveEditor `isA` gTypeScrolledWindow then do
        let textEditScroller = castToScrolledWindow currentlyActiveEditor
        textEdit <- binGetChild textEditScroller
        return $ fmap castToSourceView textEdit
    else return Nothing

getActiveEditorTab :: Context -> IO (Maybe Widget)
getActiveEditorTab ctx = do
    pageNum <- notebookGetCurrentPage tabbed
    if pageNum < 0 then
        return Nothing
    else do
        tabs <- containerGetChildren tabbed
        return $ Just $ tabs!!pageNum
    where tabbed = mainNotebook ctx

tabTitle :: Maybe FilePath -> String
tabTitle (Just filePath) = filename' filePath
    where filename' = encodeString . filename . decodeString
tabTitle Nothing = "(new file)"

tabTitleForEditor :: SourceView -> IO String
tabTitleForEditor editor = do
    quark <- fileNameQuark
    filePath <- objectGetAttributeUnsafe quark editor
    buffer <- textViewGetBuffer editor
    modified <- buffer `get` textBufferModified
    return $ if modified then tabTitle filePath ++ "*" else tabTitle filePath

updateEditorTitle :: SourceView -> IO ()
updateEditorTitle editor = do
    Just scrolledW <- widgetGetParent editor
    Just notebookW <- widgetGetParent scrolledW
    let notebook = castToNotebook notebookW
    notebookSetTabLabelText notebook scrolledW =<< tabTitleForEditor editor

setEditorFilePath :: SourceView -> Maybe FilePath -> IO ()
setEditorFilePath editor filePath = do
    quark <- fileNameQuark
    objectSetAttribute quark editor filePath

maybeDo :: (a -> IO ()) -> Maybe a -> IO ()
maybeDo = maybe (return())
