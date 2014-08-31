module Main (main) where

import           Control.Monad       (forM)
import           Control.Monad.Trans (liftIO)
import qualified Data.ByteString     as BS
import           Data.Text           (Text (..))
import qualified Data.Text.Encoding  as E
import           Data.Tree
import           Graphics.UI.Gtk
import           System.Directory
import           System.Environment  (getArgs)
import           System.FilePath
import           System.IO           (hPutStr, stderr)

import Hob.Context
import Hob.Context.FileContext  (defaultFileContext)
import Hob.Context.StyleContext (defaultStyleContext)
import Hob.DirectoryTree
import Hob.Ui

import Paths_hob

main :: IO ()
main = do
    projectRoot <- getProjectDirectory
    fileCtx <- defaultFileContext
    styleCtx <- defaultStyleContext =<< getDataDir
    let ctx = Context styleCtx fileCtx
    mainWindow <- loadGui ctx (fileTreeFromDirectory projectRoot) loadFile storeFile
    _ <- mainWindow `on` deleteEvent $ liftIO mainQuit >> return False
    widgetShowAll mainWindow
    mainGUI
    where
         getProjectDirectory = do
             args <- getArgs
             case args of
                 [] -> getCurrentDirectory
                 [path] -> canonicalizePath path
                 _ -> error "unsupported command options"


fileTreeFromDirectory :: FilePath -> IO (Forest DirectoryTreeElement)
fileTreeFromDirectory = fileTreeGenerator getDirectoryContents doesDirectoryExist

loadFile :: FileLoader
loadFile path = do
        contents <- BS.readFile path
        checkError $ E.decodeUtf8' contents
        where checkError (Left e) = do hPutStr stderr (show e)
                                       return Nothing
              checkError (Right t) = return $ Just t

storeFile :: FileWriter
storeFile path = BS.writeFile path . E.encodeUtf8
