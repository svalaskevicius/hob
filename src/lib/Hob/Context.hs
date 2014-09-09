module Hob.Context (
    Context(..)
) where

import Graphics.UI.Gtk (Entry, Notebook, TreeStore, Window)

import Hob.Context.FileContext
import Hob.Context.StyleContext
import Hob.DirectoryTree

data Context = Context {
    styleContext  :: StyleContext,
    fileContext   :: FileContext,
    mainWindow    :: Window,
    mainNotebook  :: Notebook,
    commandEntry  :: Entry,
    fileTreeStore :: TreeStore DirectoryTreeElement
}
