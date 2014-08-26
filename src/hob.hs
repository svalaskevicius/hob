module Main (main) where

import           Control.Monad       (forM)
import           Control.Monad.Trans (liftIO)
import qualified Data.ByteString     as BS
import           Data.Text           (Text (..))
import qualified Data.Text.Encoding  as E
import           Data.Tree
import           Graphics.UI.Gtk
import           Hob.DirectoryTree
import           Hob.Ui
import           Hob.Context
import           System.Directory
import           System.FilePath

import System.IO (hPutStr, stderr)

main :: IO ()
main = do
    projectRoot <- getCurrentDirectory
    let ctx = Context projectRoot
    mainWindow <- loadGui ctx (fileTreeFromDirectory projectRoot) loadFile storeFile
    _ <- mainWindow `on` deleteEvent $ liftIO mainQuit >> return False
    widgetShowAll mainWindow
    mainGUI


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
