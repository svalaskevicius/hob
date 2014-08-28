module Hob.Context (
    Context,
    defaultContext,
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
                                      SourceStyleSchemeManager,
                                      sourceLanguageManagerGuessLanguage,
                                      sourceLanguageManagerNew,
                                      sourceStyleSchemeManagerGetDefault,
                                      sourceStyleSchemeManagerGetScheme,
                                      sourceStyleSchemeManagerSetSearchPath)
import System.FilePath               (FilePath (..), (</>))

data Context = Context {
    contextDataPath        :: FilePath,
    contextLanguageManager :: SourceLanguageManager,
    contextStyleManager    :: SourceStyleSchemeManager
}

defaultContext :: FilePath -> IO Context
defaultContext dataDir = do
    languageManager <- sourceLanguageManagerNew
    styleManager <- sourceStyleSchemeManagerGetDefault
    sourceStyleSchemeManagerSetSearchPath styleManager (Just [dataDir </> "themes" </> "gtksourceview"])
    return $ Context dataDir languageManager styleManager

uiFile :: Context -> FilePath
uiFile ctx = contextDataPath ctx </> "ui.glade"

uiTheme :: Context -> FilePath
uiTheme ctx = contextDataPath ctx </> "themes" </> "gtk" </> "default" </> "gtk-dark.css"

sourceLanguage :: Context -> FilePath -> IO (Maybe SourceLanguage)
sourceLanguage ctx filePath = sourceLanguageManagerGuessLanguage (contextLanguageManager ctx) (Just filePath) (Nothing::Maybe String)

sourceStyleScheme :: Context -> Maybe FilePath -> IO (Maybe SourceStyleScheme)
sourceStyleScheme ctx _ = do
    style <- sourceStyleSchemeManagerGetScheme (contextStyleManager ctx) "molokai"
    return $ Just style

sourceStyleFont :: Context -> Maybe FilePath -> IO (Maybe FontDescription)
sourceStyleFont _ _ = liftM Just $ fontDescriptionFromString "monospace 12"
