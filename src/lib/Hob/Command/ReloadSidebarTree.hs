module Hob.Command.ReloadSidebarTree (reloadSidebarTreeCommandHandler) where
import qualified Control.Monad.State as S
import           Control.Monad.Trans (liftIO)

import Hob.Context
import Hob.Ui.Sidebar

reloadSidebarTreeCommandHandler :: CommandHandler
reloadSidebarTreeCommandHandler = CommandHandler Nothing reloadCmd

reloadCmd :: App()
reloadCmd = do
    ctx <- S.get
    liftIO $ reloadSidebarTree ctx

