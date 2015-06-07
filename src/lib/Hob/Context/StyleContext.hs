module Hob.Context.StyleContext (
    StyleContext,
    defaultStyleContext,
    uiFile,
    uiTheme,
    sourceStyleScheme,
    sourceStyleFont
) where

import           Control.Monad                 (liftM)
import           Graphics.Rendering.Pango.Font (FontDescription,
                                                fontDescriptionFromString)
import           Graphics.UI.Gtk.SourceView    (SourceStyleScheme,
                                                SourceStyleSchemeManager, sourceStyleSchemeManagerGetDefault, sourceStyleSchemeManagerGetScheme, sourceStyleSchemeManagerSetSearchPath)
import           System.FilePath               ((</>))

data StyleContext = StyleContext {
    contextDataPath     :: FilePath,
    contextStyleManager :: SourceStyleSchemeManager
}

defaultStyleContext :: FilePath -> IO StyleContext
defaultStyleContext dataDir = do
    styleManager <- sourceStyleSchemeManagerGetDefault
    sourceStyleSchemeManagerSetSearchPath styleManager (Just [dataDir </> "themes" </> "gtksourceview"])
    return $ StyleContext dataDir styleManager

uiFile :: StyleContext -> FilePath
uiFile ctx = contextDataPath ctx </> "ui.glade"

uiTheme :: StyleContext -> FilePath
uiTheme ctx = contextDataPath ctx </> "themes" </> "gtk" </> "default" </> "gtk-dark.css"

sourceStyleScheme :: StyleContext -> Maybe FilePath -> IO (Maybe SourceStyleScheme)
sourceStyleScheme ctx _ = do
    style <- sourceStyleSchemeManagerGetScheme (contextStyleManager ctx) "molokai"
    return $ Just style

sourceStyleFont :: StyleContext -> Maybe FilePath -> IO (Maybe FontDescription)
sourceStyleFont _ _ = liftM Just $ fontDescriptionFromString "Monaco 16"
