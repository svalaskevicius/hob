module Hob.Context (
    Context(..),
    uiFile,
    uiTheme,
    sourceLanguage,
    sourceStyleScheme,
    sourceStyleFont
) where

import Control.Monad                 (liftM)
import Graphics.Rendering.Pango.Font (FontDescription,
                                      fontDescriptionFromString)
import Graphics.UI.Gtk.SourceView    (SourceLanguage, SourceLanguageManager,
                                      SourceStyleScheme,
                                      sourceLanguageManagerGuessLanguage,
                                      sourceStyleSchemeManagerGetDefault,
                                      sourceStyleSchemeManagerGetScheme,
                                      sourceStyleSchemeManagerSetSearchPath)
import System.FilePath               (FilePath (..), (</>))

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

sourceStyleScheme :: Context -> Maybe FilePath -> IO (Maybe SourceStyleScheme)
sourceStyleScheme ctx _ = do
    styleManager <- sourceStyleSchemeManagerGetDefault
    sourceStyleSchemeManagerSetSearchPath styleManager (Just ["ui" </> "themes" </> "gtksourceview"])
    style <- sourceStyleSchemeManagerGetScheme styleManager "molokai"
    return $ Just style

sourceStyleFont :: Context -> Maybe FilePath -> IO (Maybe FontDescription)
sourceStyleFont _ _ = liftM Just $ fontDescriptionFromString "monospace 12"
