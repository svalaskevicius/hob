module Hob.Ui.CommandEntrySpec (main, spec) where

import Data.IORef
import Graphics.UI.Gtk
import Graphics.UI.Gtk.General.StyleContext

import           Hob.Command
import qualified Hob.Context           as HC
import qualified Hob.Context.UiContext as HC
import           Hob.Ui.CommandEntry

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
      name <- widgetGetName $ HC.commandEntry . HC.uiContext $ ctx
      name `shouldBe` "commandEntry"

    it "initially there is no error class applied" $ do
      ctx <- loadDefaultContext
      styleContext <- widgetGetStyleContext $ HC.commandEntry . HC.uiContext $ ctx
      hasErrorClass <- styleContextHasClass styleContext "error"
      hasErrorClass `shouldBe` False

    it "applies error style class if the command is unknown" $ do
      ctx <- loadDefaultContext
      let commandEntry = HC.commandEntry . HC.uiContext $ ctx
      entrySetText commandEntry "qweqwe"
      styleContext <- widgetGetStyleContext commandEntry
      hasErrorClass <- styleContextHasClass styleContext "error"
      hasErrorClass `shouldBe` True

    it "removes error style on empty command" $ do
      ctx <- loadDefaultContext
      let commandEntry = HC.commandEntry . HC.uiContext $ ctx
      entrySetText commandEntry "not empty"
      styleContext <- widgetGetStyleContext commandEntry
      styleContextAddClass styleContext "error"
      entrySetText commandEntry ""
      hasErrorClass <- styleContextHasClass styleContext "error"
      hasErrorClass `shouldBe` False

    it "removes error style on known command" $ do
      (_, commandEntry, entryApi, _) <- loadDefaultGuiWithMockedCommand
      styleContext <- widgetGetStyleContext commandEntry
      styleContextAddClass styleContext "error"
      invokePreview commandEntry entryApi "cmd->asd"
      hasErrorClass <- styleContextHasClass styleContext "error"
      hasErrorClass `shouldBe` False

    it "invokes preview on the command" $ do
      (_, commandEntry, entryApi, (_, previewReader, previewResetReader)) <- loadDefaultGuiWithMockedCommand
      invokePreview commandEntry entryApi "cmd->asd"
      previewText <- previewReader
      previewResetText <- previewResetReader
      previewText `shouldBe` Just "asd"
      previewResetText `shouldBe` Nothing

    it "resets preview before next preview" $ do
      (_, commandEntry, entryApi, (_, previewReader, previewResetReader)) <- loadDefaultGuiWithMockedCommand
      invokePreview commandEntry entryApi "cmd->asd"
      invokePreview commandEntry entryApi "cmd->as"
      previewText <- previewReader
      previewResetText <- previewResetReader
      previewText `shouldBe` Just "as"
      previewResetText `shouldBe` Just "called"

    it "executes the command" $ do
      (_, commandEntry, entryApi, (executeReader, _, _)) <- loadDefaultGuiWithMockedCommand
      invokeCommand commandEntry entryApi "cmd->asd"
      executed <- executeReader
      executed `shouldBe` Just "asd"

    it "clears command entry when executing a command" $ do
      (_, commandEntry, entryApi, _) <- loadDefaultGuiWithMockedCommand
      invokeCommand commandEntry entryApi "cmd->asd"
      text <- entryGetText commandEntry
      text `shouldBe` ""

    it "resets the last preview command before executing a command" $ do
      (_, commandEntry, entryApi, (executeReader, _, previewResetReader)) <- loadDefaultGuiWithMockedCommand
      invokeCommand commandEntry entryApi "cmd->asd"
      executed <- executeReader
      previewResetText <- previewResetReader
      executed `shouldBe` Just "asd"
      previewResetText `shouldBe` Just "called"

invokePreview :: Entry -> EntryApi -> String -> IO ()
invokePreview commandEntry api text = entrySetText commandEntry text >> fst api

invokeCommand :: Entry -> EntryApi -> String -> IO ()
invokeCommand commandEntry api text = do
    invokePreview commandEntry api text
    _ <- snd api
    return()

loadDefaultGuiWithMockedCommand :: IO (HC.Context, Entry, EntryApi, CommandHandlerReaders)
loadDefaultGuiWithMockedCommand = do
    ctx <- loadDefaultContext
    commandEntry <- entryNew
    (matcher, readHandledCommands) <- mockedMatcher "cmd->"
    entryApi <- newCommandEntryDetached ctx commandEntry matcher
    return (ctx, commandEntry, entryApi, readHandledCommands)

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
                                (\_ -> writeIORef previewCmd $ Just params)
                                (\_ -> writeIORef previewResetCmd $ Just "called"))
                            (\_ -> writeIORef cmd $ Just params),
                (
                    readIORef cmd,
                    readIORef previewCmd,
                    readIORef previewResetCmd
                )
            )
