module Hob.Control (
    maybeDo,
    flushEvents
) where

import Graphics.UI.Gtk (eventsPending, mainIteration)
import Control.Monad (when)

maybeDo :: Monad b => (a -> b ()) -> Maybe a -> b ()
maybeDo = maybe (return())

flushEvents :: IO()
flushEvents = do
    pending <- eventsPending
    when (pending > 0) $ mainIteration >> flushEvents
