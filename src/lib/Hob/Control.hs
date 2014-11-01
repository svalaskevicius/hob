module Hob.Control (
    maybeDo
) where

maybeDo :: Monad b => (a -> b ()) -> Maybe a -> b ()
maybeDo = maybe (return())
