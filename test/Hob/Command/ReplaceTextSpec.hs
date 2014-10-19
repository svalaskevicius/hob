module Hob.Command.ReplaceTextSpec (main, spec) where

import Test.Hspec
import Data.IORef
import           Data.Maybe
import qualified Hob.Context     as HC

import           Hob.Command
import           Hob.Command.ReplaceText
{-
import qualified Hob.Context.UiContext as HC
import           Hob.Ui.Editor         (newEditorForText)

import           Data.Text       (pack)
import           Graphics.UI.Gtk
import           Hob.Ui          (getActiveEditor)
-}

import HobTest.Context.Default

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "replace text command handler" $ do
    it "invokes the configured preview for the command handler" $ do
        (mockedPreview, readInvokes) <- mockedPreviewCommandHandler
        let replaceHandler = generateReplaceCommandHandler mockedPreview
        ctx <- loadDefaultContext
        (previewExecute . fromJust . commandPreview) (replaceHandler "text") ctx
        invokes <- readInvokes
        invokes `shouldBe` (["text"], 0)

    it "invokes the configured preview reset for the command handler" $ do
        (mockedPreview, readInvokes) <- mockedPreviewCommandHandler
        let replaceHandler = generateReplaceCommandHandler mockedPreview
        ctx <- loadDefaultContext
        (previewReset . fromJust . commandPreview) (replaceHandler "text") ctx
        invokes <- readInvokes
        invokes `shouldBe` ([], 1)

mockedPreviewCommandHandler :: IO (String -> PreviewCommandHandler, IO ([String], Int))
mockedPreviewCommandHandler = do
    (mockedPreview, readPreviews) <- mockedStringCommand
    (mockedReset, readResets) <- mockedCounterCommand
    let handler searchText = PreviewCommandHandler (mockedPreview searchText) mockedReset
    return (handler, do
        previews <- readPreviews
        resets <- readResets
        return (previews, resets))


mockedStringCommand :: IO (String -> HC.Context -> IO(), IO [String])
mockedStringCommand = do
    recorder <- newIORef []
    return ((\v _ -> do
            currentValues <- readIORef recorder
            writeIORef recorder $ currentValues ++ [v]),
        readIORef recorder)

mockedCounterCommand :: IO (HC.Context -> IO(), IO Int)
mockedCounterCommand = do
    recorder <- newIORef 0
    return ((\_ -> do
            currentValue <- readIORef recorder
            writeIORef recorder $ currentValue + 1),
        readIORef recorder)

