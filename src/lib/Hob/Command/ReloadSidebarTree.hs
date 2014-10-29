module Hob.Command.ReloadSidebarTree (reloadSidebarTreeCommandHandler) where

import Hob.Context
import Hob.Ui.Sidebar

reloadSidebarTreeCommandHandler :: CommandHandler
reloadSidebarTreeCommandHandler = CommandHandler Nothing reloadCmd

reloadCmd :: Command
reloadCmd ctx = do
    reloadSidebarTree ctx
    return ctx
    