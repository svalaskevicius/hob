module Hob.Ui.CommandEntrySpec (main, spec) where

import Control.Monad.Reader
import Data.IORef
import Graphics.UI.Gtk
import Graphics.UI.Gtk.General.StyleContext

import Hob.Context
import Hob.Context.UiContext
import Hob.Control
import Hob.Ui.CommandEntry

import Test.Hspec

import HobTest.Context.Default
import HobTest.Control

type CommandHandlerReaders = (IO (Maybe String), IO (Maybe String), IO (Maybe String))
type EntryApi = (App(), App())

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "command entry" $ do
    it "is named" $ do
      ctx <- loadDefaultContext
      name <- widgetGetName $ commandEntry . uiContext $ ctx
      name `shouldBe` "commandEntry"

    it "initially there is no error class applied" $ do
      ctx <- loadDefaultContext
      styleCtx <- widgetGetStyleContext $ commandEntry . uiContext $ ctx
      hasErrorClass <- styleContextHasClass styleCtx "error"
      hasErrorClass `shouldBe` False

    it "applies error style class if the command is unknown" $ do
      ctx <- loadDefaultContext
      let entry = commandEntry . uiContext $ ctx
      entrySetText entry "qweqwe"
      styleCtx <- widgetGetStyleContext entry
      flushEvents
      hasErrorClass <- styleContextHasClass styleCtx "error"
      hasErrorClass `shouldBe` True

    it "removes error style on empty command" $ do
      ctx <- loadDefaultContext
      let entry = commandEntry . uiContext $ ctx
      entrySetText entry "not empty"
      styleCtx <- widgetGetStyleContext entry
      styleContextAddClass styleCtx "error"
      entrySetText entry ""
      flushEvents
      hasErrorClass <- styleContextHasClass styleCtx "error"
      hasErrorClass `shouldBe` False

    it "removes error style on known command" $ do
      (ctx, entry, entryApi, _) <- loadDefaultGuiWithMockedCommand
      styleCtx <- widgetGetStyleContext entry
      styleContextAddClass styleCtx "error"
      runCtxActions ctx $ invokePreview entry entryApi "cmd->asd"
      hasErrorClass <- styleContextHasClass styleCtx "error"
      hasErrorClass `shouldBe` False

    it "invokes preview on the command" $ do
      (ctx, entry, entryApi, (_, previewReader, previewResetReader)) <- loadDefaultGuiWithMockedCommand
      runCtxActions ctx $ invokePreview entry entryApi "cmd->asd"
      previewText <- previewReader
      previewResetText <- previewResetReader
      previewText `shouldBe` Just "asd"
      previewResetText `shouldBe` Nothing

    it "resets preview before next preview" $ do
      (ctx, entry, entryApi, (_, previewReader, previewResetReader)) <- loadDefaultGuiWithMockedCommand
      runCtxActions ctx $ invokePreview entry entryApi "cmd->asd"
      runCtxActions ctx $ invokePreview entry entryApi "cmd->as"
      previewText <- previewReader
      previewResetText <- previewResetReader
      previewText `shouldBe` Just "as"
      previewResetText `shouldBe` Just "called"

    it "executes the command" $ do
      (ctx, entry, entryApi, (executeReader, _, _)) <- loadDefaultGuiWithMockedCommand
      runCtxActions ctx $ invokeCommand entry entryApi "cmd->asd"
      executed <- executeReader
      executed `shouldBe` Just "asd"

    it "clears command entry when executing a command" $ do
      (ctx, entry, entryApi, _) <- loadDefaultGuiWithMockedCommand
      runCtxActions ctx $ invokeCommand entry entryApi "cmd->asd"
      text <- entryGetText entry
      text `shouldBe` ""

    it "resets the last preview command before executing a command" $ do
      (ctx, entry, entryApi, (executeReader, _, previewResetReader)) <- loadDefaultGuiWithMockedCommand
      runCtxActions ctx $ invokeCommand entry entryApi "cmd->asd"
      executed <- executeReader
      previewResetText <- previewResetReader
      executed `shouldBe` Just "asd"
      previewResetText `shouldBe` Just "called"

invokePreview :: Entry -> EntryApi -> String -> App ()
invokePreview entry api text = do
    liftIO $ entrySetText entry text
    fst api

invokeCommand :: Entry -> EntryApi -> String -> App ()
invokeCommand entry api text = do
    invokePreview entry api text
    _ <- snd api
    return()

loadDefaultGuiWithMockedCommand :: IO (Context, Entry, EntryApi, CommandHandlerReaders)
loadDefaultGuiWithMockedCommand = do
    defaultCtx <- loadDefaultContext
    (matcher, readHandledCommands) <- mockedMatcher "cmd->"
    let ctx = defaultCtx{baseCommands = matcher}
    entry <- entryNew
    ret <- runApp ctx (newCommandEntryDetached entry)
    return (ctx, entry, ret, readHandledCommands)

mockedMatcher :: String -> IO (CommandMatcher, CommandHandlerReaders)
mockedMatcher prefix = do
    (handler, readHandledCommands) <- recordingHandler
    let matcher = createMatcherForPrefix prefix handler
    return (matcher, readHandledCommands)

recordingHandler :: IO (String -> CommandHandler, CommandHandlerReaders)
recordingHandler = do
    cmd <- newIORef Nothing
    previewCmd <- newIORef Nothing
    previewResetCmd <- newIORef Nothing
    return (
                \params -> CommandHandler
                            (Just $ PreviewCommandHandler
                                (liftIO $ writeIORef previewCmd $ Just params)
                                (liftIO $ writeIORef previewResetCmd $ Just "called"))
                            (liftIO $ writeIORef cmd $ Just params),
                (
                    readIORef cmd,
                    readIORef previewCmd,
                    readIORef previewResetCmd
                )
            )
