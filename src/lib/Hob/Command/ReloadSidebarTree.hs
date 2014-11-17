module Hob.Command.ReloadSidebarTree (reloadSidebarTreeCommandHandler) where
import Control.Monad.Reader

import Hob.Context
import Hob.Ui.Sidebar

reloadSidebarTreeCommandHandler :: CommandHandler
reloadSidebarTreeCommandHandler = CommandHandler Nothing reloadCmd

reloadCmd :: App()
reloadCmd = do
    ctx <- ask
    liftIO $ reloadSidebarTree ctx

