module Hob.Context (
    Context(..)
) where

import Control.Monad   (liftM)
import System.FilePath (FilePath (..), (</>))
import Graphics.UI.Gtk (Window)

import Hob.Context.FileContext
import Hob.Context.StyleContext

data Context = Context {
    styleContext :: StyleContext,
    fileContext  :: FileContext,
    mainWindow   :: Window
}
