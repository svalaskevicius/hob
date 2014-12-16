module HobTest.Context.Default (
        loadDefaultContext,
        emptyFileTree,
        emptyFileLoader,
        blackholeFileWriter
        ) where

import Data.Text (pack)

import qualified Hob.Context              as HC
import qualified Hob.Context.FileContext  as HFC
import qualified Hob.Context.StyleContext as HSC
import           Hob.Control              (flushEvents)

import Hob.Ui

blackholeFileWriter :: HFC.FileWriter
blackholeFileWriter _ _ = return ()

emptyFileTree :: HFC.FileTreeLoader
emptyFileTree = return []

emptyFileLoader :: HFC.FileLoader
emptyFileLoader _ = return $ Just $ pack ""

loadDefaultContext :: IO HC.Context
loadDefaultContext = do
    sc <- HSC.defaultStyleContext "app-data"
    fc <- HFC.defaultFileContext emptyFileLoader blackholeFileWriter emptyFileTree
    ctx <- loadGui fc sc
    flushEvents
    return ctx
