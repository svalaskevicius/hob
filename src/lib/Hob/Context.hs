module Hob.Context (
    Context(..)
) where

import Graphics.UI.Gtk (Entry, Notebook, TreeStore, Window, TreeView)

import Hob.Context.FileContext
import Hob.Context.StyleContext
import Hob.DirectoryTree

data Context = Context {
    styleContext  :: StyleContext,
    fileContext   :: FileContext,
    mainWindow    :: Window,
    mainNotebook  :: Notebook,
    commandEntry  :: Entry,
    sidebarTree   :: TreeView,
    sidebarTreeSearch :: Entry,
    fileTreeStore :: TreeStore DirectoryTreeElement
}
