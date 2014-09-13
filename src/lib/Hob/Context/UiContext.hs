module Hob.Context.UiContext (
    UiContext(..)
) where

import Graphics.UI.Gtk (Entry, Notebook, TreeView, Window)

data UiContext = UiContext {
    mainWindow        :: Window,
    mainNotebook      :: Notebook,
    commandEntry      :: Entry,
    sidebarTree       :: TreeView,
    sidebarTreeSearch :: Entry
}
