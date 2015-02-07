module Hob.Context.Events (
    initEventBus,
    registerEventHandler,
    emitEvent,
) where

import           Control.Concurrent.MVar
import           Control.Monad.Reader

import           Hob.Context.Types

initEventBus :: IO EventBus
initEventBus = do
    bus <- newMVar []
    return $ EventBus (addEventListener bus) (lookupEvent bus)
    where
        addEventListener bus event action = do
            listeners <- takeMVar bus
            putMVar bus $ combineEventListeners event action listeners

        combineEventListeners :: Event -> App() -> [(Event, [App()])] -> [(Event, [App()])]
        combineEventListeners event action [] = [(event, [action])]
        combineEventListeners event action (x@(evt, initHandlers):xs) =
            if evt == event then (evt, action:initHandlers) : xs
            else x : combineEventListeners event action xs

        lookupEvent bus event = do
            listeners <- readMVar bus
            return $ findEvent event listeners

        findEvent _ [] = []
        findEvent event ((evt, handlers):xs) = if evt == event then handlers
                                               else findEvent event xs

registerEventHandler :: Event -> App() -> App()
registerEventHandler event handler = do
    bus <- fromContext eventBus
    liftIO $ addListener bus event handler

emitEvent :: Event -> App()
emitEvent event = do
    bus <- fromContext eventBus
    handlers <- liftIO $ listenersForEvent bus event
    sequence_ handlers

