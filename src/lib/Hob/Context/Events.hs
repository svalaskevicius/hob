module Hob.Context.Events (
    initEventBus,
    registerEventHandler,
    registerParametrisedEventHandler,
    emitEvent,
    emitNamedEvent,
    emitParametrisedEvent,
) where

import           Control.Concurrent.MVar
import           Control.Monad.Reader

import           Hob.Context.Types
import Data.Typeable

eventName :: Event -> EventName
eventName (Event name) = name
eventName (EventWithParams name _) = name

initEventBus :: IO EventBus
initEventBus = do
    bus <- newMVar []
    return $ EventBus (addEventListener bus) (lookupEvent bus)
    where
        addEventListener bus event action = do
            listeners <- takeMVar bus
            putMVar bus $ combineEventListeners event action listeners

        combineEventListeners :: EventName -> EventHandler -> [(EventName, [EventHandler])] -> [(EventName, [EventHandler])]
        combineEventListeners event action [] = [(event, [action])]
        combineEventListeners event action (x@(evtName, initHandlers):xs) =
            if evtName == event then (evtName, action:initHandlers) : xs
            else x : combineEventListeners event action xs

        lookupEvent bus event = do
            listeners <- readMVar bus
            return $ findEvent event listeners

        findEvent _ [] = []
        findEvent event ((evt, handlers):xs) = if evt == event then handlers
                                               else findEvent event xs

registerEventHandler :: EventName -> EventHandler -> App()
registerEventHandler event handler = do
    bus <- fromContext eventBus
    liftIO $ addListener bus event handler

registerParametrisedEventHandler :: (Typeable a) => String -> (a -> App()) -> App()
registerParametrisedEventHandler name handler = registerEventHandler (EventName name) (EventHandler . handleParametrisedEvent $ handler)
            
handleParametrisedEvent :: (Typeable a) => (a -> App()) -> Event -> App()
handleParametrisedEvent handler (EventWithParams _ d) = do
    case cast d of 
        Just handlerData -> handler handlerData
        Nothing -> error "unexpected event data"
    return ()
handleParametrisedEvent _ _ = error "unexpected event"


emitEvent :: Event -> App()
emitEvent event = do
    bus <- fromContext eventBus
    handlers <- liftIO $ listenersForEvent bus $ eventName event
    sequence_ $ map (\(EventHandler h) -> h event) handlers

emitNamedEvent :: String -> App()
emitNamedEvent = emitEvent . Event . EventName

emitParametrisedEvent :: Typeable a => String -> a -> App()
emitParametrisedEvent name params = emitEvent $ EventWithParams (EventName name) params
