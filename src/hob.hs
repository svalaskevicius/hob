module Main where

import Control.Monad.Trans (liftIO)
import Graphics.UI.Gtk
import Hob.Ui

main :: IO ()
main = do
        mainWindow <- loadGui
        _ <- mainWindow `on` deleteEvent $ liftIO mainQuit >> return False
        widgetShowAll mainWindow
        mainGUI
