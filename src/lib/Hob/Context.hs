module Hob.Context (
    Context(..),
    defaultContext,
    sourceLanguage
) where

import Control.Monad              (liftM)
import Graphics.UI.Gtk.SourceView (SourceLanguage, SourceLanguageManager,
                                   sourceLanguageManagerGuessLanguage,
                                   sourceLanguageManagerNew)
import System.FilePath            (FilePath (..), (</>))

import Hob.Context.StyleContext

data Context = Context {
    styleContext           :: StyleContext,
    contextLanguageManager :: SourceLanguageManager
}

defaultContext :: StyleContext -> IO Context
defaultContext styleContext = do
    languageManager <- sourceLanguageManagerNew
    return $ Context styleContext languageManager

sourceLanguage :: Context -> FilePath -> IO (Maybe SourceLanguage)
sourceLanguage ctx filePath = sourceLanguageManagerGuessLanguage (contextLanguageManager ctx) (Just filePath) (Nothing::Maybe String)
