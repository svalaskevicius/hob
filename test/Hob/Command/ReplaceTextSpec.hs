module Hob.Command.ReplaceTextSpec (main, spec) where

import           Control.Monad       (replicateM_)
import           Data.Maybe
import           Data.Text           (pack)
import           Graphics.UI.Gtk
import           Test.Hspec
import Control.Monad.State (liftIO)
import Data.IORef

import           Hob.Command.ReplaceText
import           Hob.Context
import qualified Hob.Context.UiContext   as HC
import           Hob.Ui.Editor           (getActiveEditor, newEditorForText)

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
        ctx <- loadDefaultContext
        deferredRunner ctx $ commandExecute (fromJust matchResult)

    it "ignores options when parsing search and replace strings" $ do
        let handler "S" "R" = CommandHandler Nothing (return())
            handler s r = error $ "unexpected invokation with \""++s++ "\" / \""++r++"\""
            matcher = createMatcherForReplace 'x' handler
            matchResult = matchCommand matcher "x#S#R#options"
        ctx <- loadDefaultContext
        deferredRunner ctx $ commandExecute (fromJust matchResult)

    it "matches escaped separator char as text" $ do
        let handler "S/S" "R/R" = CommandHandler Nothing (return())
            handler s r = error $ "unexpected invokation with \""++s++ "\" / \""++r++"\""
            matcher = createMatcherForReplace 'x' handler
            matchResult = matchCommand matcher "x/S\\/S/R\\/R"
        ctx <- loadDefaultContext
        deferredRunner ctx $ commandExecute (fromJust matchResult)

    it "matches escaped any char as escape seq and text" $ do
        let handler "S\\xS" "R\\xR" = CommandHandler Nothing (return())
            handler s r = error $ "unexpected invokation with \""++s++ "\" / \""++r++"\""
            matcher = createMatcherForReplace 'x' handler
            matchResult = matchCommand matcher "x/S\\xS/R\\xR"
        ctx <- loadDefaultContext
        deferredRunner ctx $ commandExecute (fromJust matchResult)

    it "matches escaped end of string as text" $ do
        let handler "S" "R\\" = CommandHandler Nothing (return())
            handler s r = error $ "unexpected invokation with \""++s++ "\" / \""++r++"\""
            matcher = createMatcherForReplace 'x' handler
            matchResult = matchCommand matcher "x/S/R\\"
        ctx <- loadDefaultContext
        deferredRunner ctx $ commandExecute (fromJust matchResult)

  describe "replace text command handler" $ do
    it "focuses editor on execute" $ do
        (ctx, _) <- loadGuiWithEditor
        deferredRunner ctx $ commandExecute (replaceCommandHandler "text" ":)")
        editorFocused <- widgetGetIsFocus . fromJust =<< getActiveEditor ctx
        editorFocused `shouldBe` True

    it "does not replace if there is no highlighted text" $ do
        (ctx, buffer) <- loadGuiWithEditor
        deferredRunner ctx $ commandExecute replaceNextCommandHandler
        text <- buffer `get` textBufferText
        text `shouldBe` "text - initial text! text"

    it "replaces previously highlighted text" $ do
        (ctx, buffer) <- loadGuiWithEditor
        deferredRunner ctx $ commandExecute (replaceCommandHandler "text" ":)")
        text <- replaceNext ctx buffer
        text `shouldBe` ":) - initial text! text"

    it "does not replace if the highlighted text doesnt match the search string" $ do
        (ctx, buffer) <- loadGuiWithEditor
        deferredRunner ctx $ commandExecute (replaceCommandHandler "text" ":)")
        (s, _) <- textBufferGetSelectionBounds buffer
        e <- textBufferGetIterAtOffset buffer 7
        textBufferSelectRange buffer s e
        text <- replaceNext ctx buffer
        text `shouldBe` "text - initial text! text"

    it "sets replace mode" $ do
        (ctx, _) <- loadGuiWithEditor
        deferredRunner ctx $ commandExecute (replaceCommandHandler "text" ":)")
        ref <- newIORef Nothing
        deferredRunner ctx $ activeModes >>= (liftIO . writeIORef ref)
        modes <- readIORef ref
        (map modeName . fromJust) modes `shouldBe` ["replace"]

replaceNext :: TextBufferClass o => Context -> o -> IO String
replaceNext ctx buffer = do
    processGtkEvents
    deferredRunner ctx $ commandExecute replaceNextCommandHandler
    processGtkEvents
    buffer `get` textBufferText

loadGuiWithEditor :: IO (Context, TextBuffer)
loadGuiWithEditor = do
    ctx <- loadDefaultContext
    let notebook = HC.mainNotebook . uiContext $ ctx
    deferredRunner ctx $ newEditorForText notebook Nothing $ pack "text - initial text! text"
    mEditor <- getActiveEditor ctx
    let editor = fromJust mEditor
    buffer <- textViewGetBuffer editor
    return (ctx, buffer)

processGtkEvents :: IO ()
processGtkEvents = replicateM_ 500 $ mainIterationDo False
