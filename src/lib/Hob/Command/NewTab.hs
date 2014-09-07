module Hob.Command.NewTab (
            launchNewEditorForText,
            launchNewFileEditor,
            editNewFile,
            editNewFileCommandHandler,
            NewFileEditorLauncher) where


import Control.Monad              (filterM, (<=<))
import Data.Maybe                 (mapMaybe)
import Data.Text                  (Text, pack)
import Filesystem.Path.CurrentOS  (decodeString, encodeString, filename)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.SourceView (SourceDrawSpacesFlags (..), SourceView,
                                   castToSourceView,
                                   sourceBufferBeginNotUndoableAction,
                                   sourceBufferEndNotUndoableAction,
                                   sourceBufferNew,
                                   sourceBufferSetHighlightSyntax,
                                   sourceBufferSetLanguage,
                                   sourceBufferSetStyleScheme,
                                   sourceViewNewWithBuffer,
                                   sourceViewSetAutoIndent,
                                   sourceViewSetDrawSpaces,
                                   sourceViewSetHighlightCurrentLine,
                                   sourceViewSetIndentOnTab,
                                   sourceViewSetIndentWidth,
                                   sourceViewSetInsertSpacesInsteadOfTabs,
                                   sourceViewSetShowLineNumbers,
                                   sourceViewSetTabWidth)
import System.Glib.GObject

import Hob.Command
import Hob.Context
import Hob.Context.FileContext
import Hob.Context.StyleContext
import Hob.Control

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
              _ <- launchNewEditorForText ctx targetNotebook (Just filePath) text
              return ()
          isEditorFileMatching editor = do
              quark <- fileNameQuark
              f <- objectGetAttributeUnsafe quark editor
              return $ maybe False (filePath ==) f
          alreadyLoadedPage [(nr, _)] = Just nr
          alreadyLoadedPage _ = Nothing

launchNewEditorForText :: Context -> Notebook -> Maybe FilePath -> Text -> IO SourceView
launchNewEditorForText ctx targetNotebook filePath text = do
    buffer <- sourceBufferNew Nothing
    maybeDo (setBufferLanguage buffer <=< sourceLanguage (fileContext ctx)) filePath

    sourceBufferBeginNotUndoableAction buffer
    textBufferSetText buffer text
    textBufferSetModified buffer False
    sourceBufferEndNotUndoableAction buffer

    sourceBufferSetStyleScheme buffer =<< sourceStyleScheme (styleContext ctx) filePath

    editor <- sourceViewNewWithBuffer buffer
    sourceViewSetShowLineNumbers editor True
    sourceViewSetAutoIndent editor True
    sourceViewSetIndentOnTab editor True
    sourceViewSetIndentWidth editor 4
    sourceViewSetTabWidth editor 4
    sourceViewSetInsertSpacesInsteadOfTabs editor True
    sourceViewSetHighlightCurrentLine editor True
    sourceViewSetDrawSpaces editor SourceDrawSpacesTrailing

    scrolledWindow <- scrolledWindowNew Nothing Nothing
    scrolledWindow `containerAdd` editor

    widgetModifyFont editor =<< sourceStyleFont (styleContext ctx) filePath

    widgetShowAll scrolledWindow
    tabNr <- notebookAppendPage targetNotebook scrolledWindow title
    notebookSetCurrentPage targetNotebook tabNr
    notebookSetShowTabs targetNotebook True

    _ <- buffer `on` modifiedChanged $ notebookSetTabLabelText targetNotebook scrolledWindow =<< tabTitleForEditor editor

    setEditorFilePath editor filePath

    return editor
    where
        title = tabTitle filePath
        setBufferLanguage buffer (Just lang) = sourceBufferSetLanguage buffer (Just lang) >> sourceBufferSetHighlightSyntax buffer True
        setBufferLanguage _ Nothing = return()


editNewFile :: Context -> IO ()
editNewFile ctx = do
    _ <- launchNewEditorForText ctx tabbed Nothing $ pack ""
    return ()
    where tabbed = mainNotebook ctx



fileNameQuark :: IO Quark
fileNameQuark = quarkFromString "fileName"

getEditorFromNotebookTab :: Widget -> IO (Maybe SourceView)
getEditorFromNotebookTab currentlyActiveEditor =
    if currentlyActiveEditor `isA` gTypeScrolledWindow then do
        let textEditScroller = castToScrolledWindow currentlyActiveEditor
        textEdit <- binGetChild textEditScroller
        return $ fmap castToSourceView textEdit
    else return Nothing

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

setEditorFilePath :: SourceView -> Maybe FilePath -> IO ()
setEditorFilePath editor filePath = do
    quark <- fileNameQuark
    objectSetAttribute quark editor filePath

liftTupledMaybe :: (a, Maybe b) -> Maybe (a, b)
liftTupledMaybe (x, Just y) = Just (x, y)
liftTupledMaybe (_, Nothing) = Nothing

numberedJusts :: [Maybe a] -> [(Int, a)]
numberedJusts a = mapMaybe liftTupledMaybe $ zip [0..] a
