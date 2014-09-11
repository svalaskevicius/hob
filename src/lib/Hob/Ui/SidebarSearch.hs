module Hob.Ui.SidebarSearch (startSidebarSearch) where

import Graphics.UI.Gtk

startSidebarSearch :: a -> b -> IO Entry
startSidebarSearch _ _ = do
    entry <- entryNew
    widgetSetName entry "sidebarSearchEntry"
    return entry