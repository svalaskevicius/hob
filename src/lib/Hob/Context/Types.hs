{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Hob.Context.Types (
    App,
    EventName(..),
    EventBus(..),
    Context(..),
    PreviewCommandHandler(..),
    CommandHandler(..),
    CommandMatcher(..),
    Message(..),
    KeyboardBinding,
    KeyCommandMatcher,
    TextCommandMatcher,
    Mode(..),
    Event(..),
    EventHandler(..),
    Editor(..),
    EditorList(..),
    fromContext
    ) where

import           Control.Monad.Reader
import           Data.Maybe               (isJust)
import           Data.Monoid
import           Graphics.UI.Gtk          (Modifier, Notebook)
import           GtkExtras.LargeTreeStore as LTS (TreeStore)
import           Data.Text                  (Text)

import           Hob.Context.FileContext
import           Hob.Context.StyleContext
import           Hob.Context.UiContext
import           Hob.DirectoryTree

import Data.Typeable

type App = ReaderT Context IO

newtype EventName = EventName String deriving (Eq, Show)

data Event = Event EventName | forall a. (Typeable a) => EventWithParams EventName a

instance Show Event where
    show (Event a) = "Event: " ++ (show a)
    show (EventWithParams a _) = "Event: " ++ (show a)

newtype EventHandler = EventHandler (Event -> App())

data Editor = Editor {
    editorId           :: Editor -> App Int,
    enterEditorMode    :: Editor -> Mode -> App Editor,
    exitLastEditorMode :: Editor -> App Editor,
    modeStack          :: Editor -> App [Mode],
    isCurrentlyActive  :: Editor -> App Bool,
    getEditorFilePath  :: Editor -> App (Maybe FilePath),
    setEditorFilePath  :: Editor -> Maybe FilePath -> App Editor,
    getEditorContents  :: Editor -> App Text,
    activateEditor     :: Editor -> Notebook -> App ()
} deriving (Typeable)

data EventBus = EventBus {
    addListener       :: EventName -> EventHandler -> IO(),
    listenersForEvent :: EventName -> IO [EventHandler]
}

data EditorList = EditorList {
    updateEditors :: ([Editor] -> IO [Editor]) -> IO(),
    getEditors    :: IO [Editor]
}

data Context = Context {
    styleContext  :: StyleContext,
    fileContext   :: FileContext,
    uiContext     :: UiContext,
    fileTreeStore :: LTS.TreeStore DirectoryTreeElement,
    baseCommands  :: CommandMatcher,
    editors       :: EditorList,
    messageLoop   :: Context -> Message -> IO(),
    eventBus      :: EventBus,
    idGenerator   :: IO Int
}

data Mode = Mode {
    modeName       :: String,
    commandMatcher :: CommandMatcher,
    cleanup        :: App()
}

data Message = AppAction (App())


data PreviewCommandHandler = PreviewCommandHandler {
                    previewExecute :: App(),
                    previewReset   :: App()
                }

data CommandHandler = CommandHandler {
                    commandPreview :: Maybe PreviewCommandHandler,
                    commandExecute :: App()
                }

type KeyboardBinding = ([Modifier], String)
type SingleCommandMatcher a = a -> Maybe CommandHandler
type KeyCommandMatcher = SingleCommandMatcher KeyboardBinding
type TextCommandMatcher = SingleCommandMatcher String

data CommandMatcher = CommandMatcher {
                    matchKeyBinding :: KeyCommandMatcher,
                    matchCommand    :: TextCommandMatcher
                }

instance Monoid CommandMatcher where
    mempty = CommandMatcher (const Nothing) (const Nothing)
    mappend l r = CommandMatcher (combineMatchKeyBinding l r) (combineMatchCommand l r)
        where combineMatchKeyBinding = combineMatcher matchKeyBinding
              combineMatchCommand = combineMatcher matchCommand


fromContext :: MonadReader r m => (r -> a) -> m a
fromContext = asks


combineMatcher :: (CommandMatcher -> SingleCommandMatcher a) -> CommandMatcher -> CommandMatcher -> SingleCommandMatcher a
combineMatcher combiner l r cmd = if isJust rightResult then rightResult else leftResult
    where leftResult = combiner l cmd
          rightResult = combiner r cmd
