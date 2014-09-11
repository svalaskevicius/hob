module Hob.Command.ReloadSidebarTree (reloadSidebarTreeCommandHandler) where

import Hob.Command
import Hob.Ui.Sidebar

reloadSidebarTreeCommandHandler :: CommandHandler
reloadSidebarTreeCommandHandler = CommandHandler Nothing reloadSidebarTree
