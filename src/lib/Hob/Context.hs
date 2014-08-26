module Hob.Context (Context(..), uiFile) where

import System.FilePath (FilePath(..), (</>))

data Context = Context {
    basePath :: FilePath
}

uiFile :: Context -> FilePath
uiFile ctx = basePath ctx </> "ui/ui.glade"
