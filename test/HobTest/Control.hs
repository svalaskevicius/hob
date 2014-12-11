module HobTest.Control (
    runCtxActions
) where

import Hob.Context
import Hob.Control (flushEvents)

runCtxActions :: Context -> App() -> IO()
runCtxActions ctx actions = do
    deferredRunner ctx actions
    flushEvents
