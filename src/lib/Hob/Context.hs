module Hob.Context (
    Context(..),
    uiFile,
    uiTheme,
    sourceLanguage
) where

import Graphics.UI.Gtk.SourceView (SourceLanguage, SourceLanguageManager,
                                   sourceLanguageManagerGuessLanguage)
import System.FilePath            (FilePath (..), (</>))

data Context = Context {
    contextBasePath        :: FilePath,
    contextLanguageManager :: SourceLanguageManager
}

uiFile :: Context -> FilePath
uiFile ctx = contextBasePath ctx </> "ui" </> "ui.glade"

uiTheme :: Context -> FilePath
uiTheme ctx = contextBasePath ctx </> "ui" </> "themes" </> "gtk" </> "default" </> "gtk-dark.css"

sourceLanguage :: Context -> FilePath -> IO (Maybe SourceLanguage)
sourceLanguage ctx filePath = sourceLanguageManagerGuessLanguage (contextLanguageManager ctx) (Just filePath) (Nothing::Maybe String)
