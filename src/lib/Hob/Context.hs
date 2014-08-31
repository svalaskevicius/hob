module Hob.Context (
    Context(..)
) where

import Control.Monad   (liftM)
import System.FilePath (FilePath (..), (</>))

import Hob.Context.FileContext
import Hob.Context.StyleContext

data Context = Context {
    styleContext :: StyleContext,
    fileContext  :: FileContext
}
