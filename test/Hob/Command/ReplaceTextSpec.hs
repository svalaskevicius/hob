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
  describe "replace text command matcher" $ do
    it "accepts when command starts with the configured letter" $ do
        let handler _ _ = CommandHandler Nothing (\_ -> return())
        let matcher = createMatcherForReplace 's' handler
        isJust (matchCommand matcher "s//") `shouldBe` True

    it "does not accept when command starts with another than the configured letter" $ do
        let handler _ _ = CommandHandler Nothing (\_ -> return())
        let matcher = createMatcherForReplace 's' handler
        isJust (matchCommand matcher "x//") `shouldBe` False

    it "does not accept when command is empty" $ do
        let handler _ _ = CommandHandler Nothing (\_ -> return())
        let matcher = createMatcherForReplace 's' handler
        isJust (matchCommand matcher "") `shouldBe` False
        
    it "parses search and replace strings" $ do
        let handler "S" "R" = CommandHandler Nothing (\_ -> return())
            handler s r = error $ "unexpected invokation with \""++s++ "\" / \""++r++"\""
            matcher = createMatcherForReplace 'x' handler
            matchResult = matchCommand matcher "x/S/R"
        commandExecute (fromJust matchResult) =<< loadDefaultContext

    it "ignores options when parsing search and replace strings" $ do
        let handler "S" "R" = CommandHandler Nothing (\_ -> return())
            handler s r = error $ "unexpected invokation with \""++s++ "\" / \""++r++"\""
            matcher = createMatcherForReplace 'x' handler
            matchResult = matchCommand matcher "x#S#R#options"
        commandExecute (fromJust matchResult) =<< loadDefaultContext

    it "matches escaped separator char as text" $ do
        let handler "S/S" "R/R" = CommandHandler Nothing (\_ -> return())
            handler s r = error $ "unexpected invokation with \""++s++ "\" / \""++r++"\""
            matcher = createMatcherForReplace 'x' handler
            matchResult = matchCommand matcher "x/S\\/S/R\\/R"
        commandExecute (fromJust matchResult) =<< loadDefaultContext

    it "matches escaped any char as escape seq and text" $ do
        let handler "S\\xS" "R\\xR" = CommandHandler Nothing (\_ -> return())
            handler s r = error $ "unexpected invokation with \""++s++ "\" / \""++r++"\""
            matcher = createMatcherForReplace 'x' handler
            matchResult = matchCommand matcher "x/S\\xS/R\\xR"
        commandExecute (fromJust matchResult) =<< loadDefaultContext

    it "matches escaped end of string as text" $ do
        let handler "S" "R\\" = CommandHandler Nothing (\_ -> return())
            handler s r = error $ "unexpected invokation with \""++s++ "\" / \""++r++"\""
            matcher = createMatcherForReplace 'x' handler
            matchResult = matchCommand matcher "x/S/R\\"
        commandExecute (fromJust matchResult) =<< loadDefaultContext

  describe "replace text command handler" $ do
    it "invokes the configured preview for the command handler" $ do
        (replaceHandler, readPreviews, _) <- mockedReplaceCommandHandler
        ctx <- loadDefaultContext
        (previewExecute . fromJust . commandPreview) (replaceHandler "text") ctx
        invokes <- readPreviews
        invokes `shouldBe` (["text"], 0)

    it "invokes the configured preview reset for the command handler" $ do
        (replaceHandler, readPreviews, _) <- mockedReplaceCommandHandler
        ctx <- loadDefaultContext
        (previewReset . fromJust . commandPreview) (replaceHandler "text") ctx
        invokes <- readPreviews
        invokes `shouldBe` ([], 1)
        
    it "invokes the configured command on execute" $ do
        (replaceHandler, _, readExecutes) <- mockedReplaceCommandHandler
        ctx <- loadDefaultContext
        commandExecute (replaceHandler "text") ctx
        invokes <- readExecutes
        invokes `shouldBe` ["text"]
     
    it "invokes the configured command on execute next" $ do
        (replaceNextHandler, readExecutes) <- mockedReplaceNextCommandHandler
        ctx <- loadDefaultContext
        commandExecute (replaceNextHandler) ctx
        invokes <- readExecutes
        invokes `shouldBe` 1
     
    

mockedReplaceCommandHandler :: IO (String -> CommandHandler, IO ([String], Int), IO [String])
mockedReplaceCommandHandler = do    
    (mockedPreview, readPreviews) <- mockedPreviewCommandHandler
    (mockedExecute, readExecutes) <- mockedStringCommand
    let replaceHandler = generateReplaceCommandHandler mockedPreview mockedExecute
    return (replaceHandler, readPreviews, readExecutes)

mockedReplaceNextCommandHandler :: IO (CommandHandler, IO Int)
mockedReplaceNextCommandHandler = do
    (replaceNextCommand, readExecutes) <- mockedCounterCommand
    return ((generateReplaceNextCommandHandler replaceNextCommand), readExecutes)

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

