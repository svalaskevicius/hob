module Hob.Ui.CommandEntrySpec (main, spec) where

import Data.IORef
import Graphics.UI.Gtk
import Graphics.UI.Gtk.General.StyleContext

import Hob.Context
import Hob.Context.UiContext
import Hob.Ui.CommandEntry

import Test.Hspec

import HobTest.Context.Default

type CommandHandlerReaders = (IO (Maybe String), IO (Maybe String), IO (Maybe String))
type EntryApi = (IO(), IO Bool)

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
      hasErrorClass <- styleContextHasClass styleCtx "error"
      hasErrorClass `shouldBe` True

    it "removes error style on empty command" $ do
      ctx <- loadDefaultContext
      let entry = commandEntry . uiContext $ ctx
      entrySetText entry "not empty"
      styleCtx <- widgetGetStyleContext entry
      styleContextAddClass styleCtx "error"
      entrySetText entry ""
      hasErrorClass <- styleContextHasClass styleCtx "error"
      hasErrorClass `shouldBe` False

    it "removes error style on known command" $ do
      (_, entry, entryApi, _) <- loadDefaultGuiWithMockedCommand
      styleCtx <- widgetGetStyleContext entry
      styleContextAddClass styleCtx "error"
      invokePreview entry entryApi "cmd->asd"
      hasErrorClass <- styleContextHasClass styleCtx "error"
      hasErrorClass `shouldBe` False

    it "invokes preview on the command" $ do
      (_, entry, entryApi, (_, previewReader, previewResetReader)) <- loadDefaultGuiWithMockedCommand
      invokePreview entry entryApi "cmd->asd"
      previewText <- previewReader
      previewResetText <- previewResetReader
      previewText `shouldBe` Just "asd"
      previewResetText `shouldBe` Nothing

    it "resets preview before next preview" $ do
      (_, entry, entryApi, (_, previewReader, previewResetReader)) <- loadDefaultGuiWithMockedCommand
      invokePreview entry entryApi "cmd->asd"
      invokePreview entry entryApi "cmd->as"
      previewText <- previewReader
      previewResetText <- previewResetReader
      previewText `shouldBe` Just "as"
      previewResetText `shouldBe` Just "called"

    it "executes the command" $ do
      (_, entry, entryApi, (executeReader, _, _)) <- loadDefaultGuiWithMockedCommand
      invokeCommand entry entryApi "cmd->asd"
      executed <- executeReader
      executed `shouldBe` Just "asd"

    it "clears command entry when executing a command" $ do
      (_, entry, entryApi, _) <- loadDefaultGuiWithMockedCommand
      invokeCommand entry entryApi "cmd->asd"
      text <- entryGetText entry
      text `shouldBe` ""

    it "resets the last preview command before executing a command" $ do
      (_, entry, entryApi, (executeReader, _, previewResetReader)) <- loadDefaultGuiWithMockedCommand
      invokeCommand entry entryApi "cmd->asd"
      executed <- executeReader
      previewResetText <- previewResetReader
      executed `shouldBe` Just "asd"
      previewResetText `shouldBe` Just "called"

invokePreview :: Entry -> EntryApi -> String -> IO ()
invokePreview entry api text = entrySetText entry text >> fst api

invokeCommand :: Entry -> EntryApi -> String -> IO ()
invokeCommand entry api text = do
    invokePreview entry api text
    _ <- snd api
    return()

loadDefaultGuiWithMockedCommand :: IO (Context, Entry, EntryApi, CommandHandlerReaders)
loadDefaultGuiWithMockedCommand = do
    ctx <- loadDefaultContext
    entry <- entryNew
    (matcher, readHandledCommands) <- mockedMatcher "cmd->"
    entryApi <- newCommandEntryDetached ctx entry matcher
    return (ctx, entry, entryApi, readHandledCommands)

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
                                (\ctx -> (writeIORef previewCmd $ Just params) >> return ctx)
                                (\ctx -> (writeIORef previewResetCmd $ Just "called") >> return ctx))
                            (\ctx -> (writeIORef cmd $ Just params) >> return ctx),
                (
                    readIORef cmd,
                    readIORef previewCmd,
                    readIORef previewResetCmd
                )
            )
