module Hob.Command.ReloadSidebarTree (reloadSidebarTreeCommandHandler) where

import Data.Maybe
import Graphics.UI.Gtk


import Hob.Command
import Hob.Context
import Hob.Context.FileContext

reloadSidebarTreeCommandHandler :: CommandHandler
reloadSidebarTreeCommandHandler = CommandHandler Nothing reloadSidebarTree

reloadSidebarTree :: Context -> IO ()
reloadSidebarTree ctx = do
    let treeStore = fileTreeStore ctx
    let fileCtx = fileContext ctx
    let fileTreeLoader = contextFileTreeLoader fileCtx
    treeStoreClear treeStore
    treeStoreInsertForest treeStore [] 0 =<< fileTreeLoader
