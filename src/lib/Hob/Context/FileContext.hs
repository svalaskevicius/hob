module Hob.Context.FileContext (
    FileContext(..),
    FileLoader,
    FileWriter,
    FileTreeLoader,
    defaultFileContext,
    sourceLanguage
) where

import Control.Monad              (liftM)
import Data.Text                  (Text)
import Data.Tree                  (Forest)
import Graphics.UI.Gtk.SourceView (SourceLanguage, SourceLanguageManager,
                                   sourceLanguageManagerGuessLanguage,
                                   sourceLanguageManagerNew)
import System.FilePath            (FilePath (..), (</>))

import Hob.DirectoryTree

type FileLoader = FilePath -> IO (Maybe Text)
type FileWriter = FilePath -> Text -> IO ()
type FileTreeLoader = IO (Forest DirectoryTreeElement)

data FileContext = FileContext {
    contextLanguageManager :: SourceLanguageManager,
    contextFileLoader      :: FileLoader,
    contextFileWriter      :: FileWriter,
    contextFileTreeLoader  :: FileTreeLoader
}

defaultFileContext :: FileLoader -> FileWriter -> FileTreeLoader -> IO FileContext
defaultFileContext loader writer treeLoader = do
    languageManager <- sourceLanguageManagerNew
    return $ FileContext languageManager loader writer treeLoader

sourceLanguage :: FileContext -> FilePath -> IO (Maybe SourceLanguage)
sourceLanguage ctx filePath = sourceLanguageManagerGuessLanguage (contextLanguageManager ctx) (Just filePath) (Nothing::Maybe String)
