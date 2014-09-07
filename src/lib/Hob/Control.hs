module Hob.Control (
    maybeDo
) where

maybeDo :: (a -> IO ()) -> Maybe a -> IO ()
maybeDo = maybe (return())
