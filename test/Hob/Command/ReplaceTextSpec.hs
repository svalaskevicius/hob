module Hob.Command.ReplaceTextSpec (main, spec) where

import           Control.Monad       (replicateM_)
import qualified Control.Monad.State as S
import           Data.IORef
import           Data.Maybe
import           Data.Text           (pack)
import           Graphics.UI.Gtk
import           Test.Hspec

import           Hob.Command.ReplaceText
import           Hob.Context
import qualified Hob.Context.UiContext   as HC
import           Hob.Ui.Editor           (newEditorForText)
--import           Hob.Ui          (getActiveEditor)

import HobTest.Context.Default

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "replace text command matcher" $ do
    it "accepts when command starts with the configured letter" $ do
        let handler _ _ = CommandHandler Nothing (return())
        let matcher = createMatcherForReplace 's' handler
        isJust (matchCommand matcher "s//") `shouldBe` True

    it "does not accept when command starts with another than the configured letter" $ do
        let handler _ _ = CommandHandler Nothing (return())
        let matcher = createMatcherForReplace 's' handler
        isJust (matchCommand matcher "x//") `shouldBe` False

    it "does not accept when command is empty" $ do
        let handler _ _ = CommandHandler Nothing (return())
        let matcher = createMatcherForReplace 's' handler
        isJust (matchCommand matcher "") `shouldBe` False

    it "parses search and replace strings" $ do
        let handler "S" "R" = CommandHandler Nothing (return())
            handler s r = error $ "unexpected invokation with \""++s++ "\" / \""++r++"\""
            matcher = createMatcherForReplace 'x' handler
            matchResult = matchCommand matcher "x/S/R"
        _ <- runApp (commandExecute (fromJust matchResult)) =<< loadDefaultContext
        return ()

    it "ignores options when parsing search and replace strings" $ do
        let handler "S" "R" = CommandHandler Nothing (return())
            handler s r = error $ "unexpected invokation with \""++s++ "\" / \""++r++"\""
            matcher = createMatcherForReplace 'x' handler
            matchResult = matchCommand matcher "x#S#R#options"
        _ <- runApp (commandExecute (fromJust matchResult)) =<< loadDefaultContext
        return ()

    it "matches escaped separator char as text" $ do
        let handler "S/S" "R/R" = CommandHandler Nothing (return())
            handler s r = error $ "unexpected invokation with \""++s++ "\" / \""++r++"\""
            matcher = createMatcherForReplace 'x' handler
            matchResult = matchCommand matcher "x/S\\/S/R\\/R"
        _ <- runApp (commandExecute (fromJust matchResult)) =<< loadDefaultContext
        return ()

    it "matches escaped any char as escape seq and text" $ do
        let handler "S\\xS" "R\\xR" = CommandHandler Nothing (return())
            handler s r = error $ "unexpected invokation with \""++s++ "\" / \""++r++"\""
            matcher = createMatcherForReplace 'x' handler
            matchResult = matchCommand matcher "x/S\\xS/R\\xR"
        _ <- runApp (commandExecute (fromJust matchResult)) =<< loadDefaultContext
        return ()

    it "matches escaped end of string as text" $ do
        let handler "S" "R\\" = CommandHandler Nothing (return())
            handler s r = error $ "unexpected invokation with \""++s++ "\" / \""++r++"\""
            matcher = createMatcherForReplace 'x' handler
            matchResult = matchCommand matcher "x/S/R\\"
        _ <- runApp (commandExecute (fromJust matchResult)) =<< loadDefaultContext
        return ()

  describe "replace text command handler" $ do
    it "invokes the configured preview for the command handler" $ do
        (replaceHandler, readPreviews, _) <- mockedReplaceCommandHandler
        ctx <- loadDefaultContext
        _ <- runApp ((previewExecute . fromJust . commandPreview) (replaceHandler "text" "newtext")) ctx
        invokes <- readPreviews
        invokes `shouldBe` (["text"], 0)

    it "invokes the configured preview reset for the command handler" $ do
        (replaceHandler, readPreviews, _) <- mockedReplaceCommandHandler
        ctx <- loadDefaultContext
        _ <- runApp ((previewReset . fromJust . commandPreview) (replaceHandler "text" "newtext")) ctx
        invokes <- readPreviews
        invokes `shouldBe` ([], 1)

    it "invokes the configured command on execute" $ do
        (replaceHandler, _, readExecutes) <- mockedReplaceCommandHandler
        ctx <- loadDefaultContext
        _ <- runApp (commandExecute (replaceHandler "text" "newtext")) ctx
        invokes <- readExecutes
        invokes `shouldBe` ["text"]

    it "invokes the configured command on execute next" $ do
        (replaceNextHandler, readExecutes) <- mockedReplaceNextCommandHandler
        ctx <- loadDefaultContext
        _ <- runApp (commandExecute replaceNextHandler) ctx
        invokes <- readExecutes
        invokes `shouldBe` 1

    it "does not replace if there is no highlighted text" $ do
        (ctx, buffer) <- loadGuiWithEditor
        _ <- runApp (commandExecute replaceNextCommandHandler) ctx
        text <- buffer `get` textBufferText
        text `shouldBe` "text - initial text! text"

    it "replaces previously highlighted text" $ do
        (ctx, buffer) <- loadGuiWithEditor
        _ <- runApp (commandExecute (replaceCommandHandler "text" ":)")) ctx
        text <- replaceNext ctx buffer
        text `shouldBe` ":) - initial text! text"

    it "does not replace if the highlighted text doesnt match the search string" $ do
        (ctx, buffer) <- loadGuiWithEditor
        _ <- runApp (commandExecute (replaceCommandHandler "text" ":)")) ctx
        (s, _) <- textBufferGetSelectionBounds buffer
        e <- textBufferGetIterAtOffset buffer 7
        textBufferSelectRange buffer s e
        text <- replaceNext ctx buffer
        text `shouldBe` "text - initial text! text"

replaceNext :: TextBufferClass o => Context -> o -> IO String
replaceNext ctx buffer = do
    processGtkEvents
    _ <- runApp (commandExecute replaceNextCommandHandler) ctx
    processGtkEvents
    buffer `get` textBufferText

loadGuiWithEditor :: IO (Context, TextBuffer)
loadGuiWithEditor = do
    ctx <- loadDefaultContext
    let notebook = HC.mainNotebook . uiContext $ ctx
    editor <- newEditorForText ctx notebook Nothing $ pack "text - initial text! text"
    buffer <- textViewGetBuffer editor
    return (ctx, buffer)

mockedReplaceCommandHandler :: IO (String -> String -> CommandHandler, IO ([String], Int), IO [String])
mockedReplaceCommandHandler = do
    (mockedPreview, readPreviews) <- mockedPreviewCommandHandler
    (mockedExecute, readExecutes) <- mockedStringCommand
    let replaceHandler = generateReplaceCommandHandler mockedPreview mockedExecute
    return (replaceHandler, readPreviews, readExecutes)

mockedReplaceNextCommandHandler :: IO (CommandHandler, IO Int)
mockedReplaceNextCommandHandler = do
    (replaceNextCommand, readExecutes) <- mockedCounterCommand
    return (generateReplaceNextCommandHandler replaceNextCommand, readExecutes)

mockedPreviewCommandHandler :: IO (String -> PreviewCommandHandler, IO ([String], Int))
mockedPreviewCommandHandler = do
    (mockedPreview, readPreviews) <- mockedStringCommand
    (mockedReset, readResets) <- mockedCounterCommand
    let handler searchText = PreviewCommandHandler (mockedPreview searchText) mockedReset
    return (handler, do
        previews <- readPreviews
        resets <- readResets
        return (previews, resets))

mockedStringCommand :: IO (String -> Command, IO [String])
mockedStringCommand = do
    recorder <- newIORef []
    return (\v -> S.liftIO $ do
            currentValues <- readIORef recorder
            writeIORef recorder $ currentValues ++ [v],
        readIORef recorder)

mockedCounterCommand :: IO (Command, IO Int)
mockedCounterCommand = do
    recorder <- newIORef 0
    return (S.liftIO $ do
            currentValue <- readIORef recorder
            writeIORef recorder $ currentValue + 1,
        readIORef recorder)

processGtkEvents :: IO ()
processGtkEvents = replicateM_ 500 $ mainIterationDo False
