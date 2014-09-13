module Main (main) where

import           Control.Monad.Trans (liftIO)
import qualified Data.ByteString     as BS
import qualified Data.Text.Encoding  as E
import           Graphics.UI.Gtk
import           System.Directory
import           System.Environment  (getArgs)
import           System.IO           (hPutStr, stderr)

import Hob.Context
import Hob.Context.FileContext  (FileLoader, FileTreeLoader, FileWriter,
                                 defaultFileContext)
import Hob.Context.StyleContext (defaultStyleContext)
import Hob.DirectoryTree
import Hob.Ui

import Paths_hob

main :: IO ()
main = do
    fileCtx <- getFileContext
    styleCtx <- getStyleContext
    ctx <- loadGui fileCtx styleCtx
    _ <- mainWindow ctx `on` deleteEvent $ liftIO mainQuit >> return False
    widgetShow $ mainWindow ctx
    mainGUI
    where
         getFileContext = do
             projectRoot <- getProjectDirectory
             defaultFileContext loadFile storeFile (fileTreeFromDirectory projectRoot)
         getStyleContext = defaultStyleContext =<< getDataDir
         getProjectDirectory = do
             args <- getArgs
             case args of
                 [] -> getCurrentDirectory
                 [path] -> canonicalizePath path
                 _ -> error "unsupported command options"


fileTreeFromDirectory :: FilePath -> FileTreeLoader
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
