module Hob.Command.ReloadSidebarTree (reloadSidebarTreeCommandHandler) where

import           Hob.Context
import           Hob.Ui.Sidebar

reloadSidebarTreeCommandHandler :: CommandHandler
reloadSidebarTreeCommandHandler = CommandHandler Nothing reloadCmd

reloadCmd :: App()
reloadCmd = reloadSidebarTree

