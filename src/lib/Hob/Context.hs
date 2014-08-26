module Hob.Context (Context(..), uiFile, uiTheme) where

import System.FilePath (FilePath (..), (</>))

data Context = Context {
    basePath :: FilePath
}

uiFile :: Context -> FilePath
uiFile ctx = basePath ctx </> "ui" </> "ui.glade"

uiTheme :: Context -> FilePath
uiTheme ctx = basePath ctx </> "ui" </> "themes" </> "gtk" </> "default" </> "gtk-dark.css"
