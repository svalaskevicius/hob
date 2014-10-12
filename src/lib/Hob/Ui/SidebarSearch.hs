module Hob.Ui.SidebarSearch (
        newSideBarFileTreeSearch,
        startSidebarSearch,
        updateSidebarSearch,
        continueSidebarSearch,
        continueSidebarSearchBackwards
    ) where

import           Control.Monad.Trans                  (liftIO)
import           Data.Char                            (isPrint, toLower)
import qualified Data.Function                        as Fnc (on)
import           Data.List                            (minimumBy, tails)
import           Data.Maybe                           (fromJust, isJust)
import           Data.Text                            (unpack)
import           Graphics.UI.Gtk
import           Graphics.UI.Gtk.General.StyleContext (styleContextAddClass,
                                                       styleContextRemoveClass)

import Hob.Context
import Hob.Context.UiContext
import Hob.Control
import Hob.Ui.Sidebar        (activateSidebarPath, nameColumn, pathColumn)

newSideBarFileTreeSearch :: Context -> IO ()
newSideBarFileTreeSearch ctx = do
    let treeView = sidebarTree.uiContext $ ctx
    let searchEntry = sidebarTreeSearch.uiContext $ ctx
    _ <- treeView `on` keyPressEvent $ do
        key <- eventKeyVal
        maybe (return False) startSearch $ keyToChar key
    _ <- searchEntry `on` editableChanged $ updateSidebarSearch ctx
    _ <- searchEntry `on` focusOutEvent $ liftIO $ widgetHide searchEntry >> return False
    _ <- searchEntry `on` keyPressEvent $ do
        modifier <- eventModifier
        if null modifier then do
            key <- eventKeyName
            case unpack key of
                "Down" -> liftIO $ continueSidebarSearch ctx >> return True
                "Up" -> liftIO $ continueSidebarSearchBackwards ctx >> return True
                "Return" -> stopSearchAndActivateResult treeView searchEntry
                _ -> return False
        else return False
    return ()
    where
        startSearch firstChar
            | isPrint firstChar = liftIO $ do
                startSidebarSearch ctx [firstChar]
                return True
            | otherwise = return False
        stopSearchAndActivateResult treeView searchEntry = liftIO $ do
            widgetGrabFocus treeView
            widgetHide searchEntry
            (path, _) <- treeViewGetCursor treeView
            column <- treeViewGetColumn treeView 0
            maybeDo (treeViewRowActivated treeView path) column
            return True

startSidebarSearch :: Context -> String -> IO ()
startSidebarSearch ctx searchString = do
    let entry = sidebarTreeSearch.uiContext $ ctx
        sidebar = sidebarTree . uiContext $ ctx
        len = length searchString
    treeViewSetCursor sidebar [] Nothing
    entrySetText entry ""
    entrySetText entry searchString
    widgetShow entry
    widgetGrabFocus entry
    editableSelectRegion entry len len

updateSidebarSearch :: Context -> IO ()
updateSidebarSearch ctx = invokeOnTreeViewAndModel continueSearch ctx
    where
        continueSearch treeView model = do
            let searchEntry = sidebarTreeSearch.uiContext $ ctx
            searchString <- entryGetText searchEntry
            maybeFirstIter <- iterOnSelection treeView model
            maybeDo (selectNextMatch treeView model searchEntry searchString) maybeFirstIter

        iterOnSelection treeView model = do
            (path, _) <- treeViewGetCursor treeView
            selectedIter <- treeModelGetIter model path
            maybe (treeModelGetIterFirst model) (return.Just) selectedIter

continueSidebarSearch :: Context -> IO ()
continueSidebarSearch ctx = invokeOnTreeViewAndModel continueSearch ctx
    where
        continueSearch treeView model = do
            let searchEntry = sidebarTreeSearch.uiContext $ ctx
            searchString <- entryGetText searchEntry
            maybeFirstIter <- iterAfterSelection treeView model
            maybeDo (selectNextMatch treeView model searchEntry searchString) maybeFirstIter

        iterAfterSelection treeView model = do
            (path, _) <- treeViewGetCursor treeView
            currentIter <- treeModelGetIter model path
            maybe (return Nothing) (findNextSubtree model) currentIter

continueSidebarSearchBackwards :: Context -> IO ()
continueSidebarSearchBackwards ctx = invokeOnTreeViewAndModel continueSearch ctx
    where
        continueSearch treeView model = do
            let searchEntry = sidebarTreeSearch.uiContext $ ctx
            searchString <- entryGetText searchEntry
            maybeFirstIter <- iterBeforeSelection treeView model
            maybeDo (selectPreviousMatch treeView model searchEntry searchString) maybeFirstIter

        iterBeforeSelection treeView model = do
            (path, _) <- treeViewGetCursor treeView
            currentIter <- treeModelGetIter model path
            maybe (return Nothing) (findPreviousSubtree model) currentIter

invokeOnTreeViewAndModel :: (TreeView -> TreeModel -> IO ()) -> Context -> IO ()
invokeOnTreeViewAndModel fnc ctx = do
    let treeView = sidebarTree.uiContext $ ctx
    model <- treeViewGetModel treeView
    maybeDo (fnc treeView) model

selectNextMatch :: (TreeViewClass tv, TreeModelClass tm, EntryClass e) => tv -> tm -> e -> String -> TreeIter -> IO ()
selectNextMatch treeView treeModel =
    selectMatch (findNextSubtree treeModel) (treeModelIterChildren treeModel) (treeModelIterNext treeModel) treeView treeModel

selectPreviousMatch :: (TreeViewClass tv, TreeModelClass tm, EntryClass e) => tv -> tm -> e -> String -> TreeIter -> IO ()
selectPreviousMatch treeView treeModel =
    selectMatch (findPreviousSubtree treeModel) (treeModelIterLastChild treeModel) (treeModelIterPrevious treeModel) treeView treeModel

selectMatch :: (TreeViewClass tv, TreeModelClass tm, EntryClass e) =>
                (TreeIter -> IO (Maybe TreeIter)) -> (TreeIter -> IO (Maybe TreeIter)) -> (TreeIter -> IO (Maybe TreeIter)) ->
                tv -> tm -> e -> String -> TreeIter -> IO ()
selectMatch findNextSubTreeToMatch findFirstChildToMatch findNextChildToMatch
                treeView treeModel searchEntry searchString currentIter = do
    maybePath <- findMatchingPath treeModel searchString currentIter
    maybe (setErrorState searchEntry) (\tp -> unsetErrorState searchEntry >> activateSidebarPath treeView tp) maybePath
    where
        findMatchingPath model search iter = do
            subtreeSearch <- eatParentMatches model search iter
            subtreeMatches <- findMatchingPathInSubtree model subtreeSearch iter
            if isJust subtreeMatches then return subtreeMatches
            else maybe (return Nothing) (findMatchingPath model search) =<< findNextSubTreeToMatch iter

        findMatchingPathInSubtree model search iter = do
            firstChildIter <- findFirstChildToMatch iter
            value <- treeModelGetValue model iter nameColumn
            let newSearchString = search `eatMatcherFrom` ('/':value)
            if isJust firstChildIter then do
                ret <- findMatchingPathInSubtree model newSearchString $ fromJust firstChildIter
                maybe (findMatchingSibling model search iter) (return.Just) ret
            else
                if newSearchString == "" then do
                    path <- treeModelGetPath model iter
                    return $ Just path
                else findMatchingSibling model search iter

        findMatchingSibling model search iter =
            maybe (return Nothing) (findMatchingPathInSubtree model search) =<< findNextChildToMatch iter

setErrorState :: EntryClass e => e -> IO ()
setErrorState searchEntry = do
    widgetStyleContext <- widgetGetStyleContext searchEntry
    styleContextAddClass widgetStyleContext "error"

unsetErrorState :: EntryClass e => e -> IO ()
unsetErrorState searchEntry = do
    widgetStyleContext <- widgetGetStyleContext searchEntry
    styleContextRemoveClass widgetStyleContext "error"

eatParentMatches :: TreeModelClass treeModel => treeModel -> String -> TreeIter -> IO String
eatParentMatches model search iter = do
    path <- treeModelGetValue model iter pathColumn
    let parentPath = reverse . dropWhile (/= '/') . reverse $ path
    return $ eatMatcherFrom search parentPath

eatMatcherFrom :: String -> String -> String
eatMatcherFrom = eatMatcherLoosely
    where eatMatcherLoosely [] _ = []
          eatMatcherLoosely search [] = search
          eatMatcherLoosely (s:search) (v:value)
            | s `matches` v = maximiseStrictMatch (s:search) (v:value)
            | s == ' ' = eatMatcherLoosely search value
            | otherwise = eatMatcherLoosely (s:search) value
          eatMatcherStrictly [] _ = []
          eatMatcherStrictly search [] = search
          eatMatcherStrictly (s:search) (v:value)
            | s == ' ' = eatMatcherLoosely search (v:value)
            | s `matches` v = eatMatcherStrictly search value
            | otherwise = s:search
          maximiseStrictMatch search value = let
                  searches = [eatMatcherStrictly search val | val <- tails value]
                  searchesWithLength = [(s, length s) | s <- searches]
              in fst $ minimumBy (compare `Fnc.on` snd) searchesWithLength
          matches s v = toLower s == toLower v

findNextSubtree :: TreeModelClass treeModel => treeModel -> TreeIter -> IO (Maybe TreeIter)
findNextSubtree model iter = do
    next <- treeModelIterNext model iter
    if isJust next then return next
    else maybe (return Nothing) (findNextSubtree model) =<< treeModelIterParent model iter

findPreviousSubtree :: TreeModelClass treeModel => treeModel -> TreeIter -> IO (Maybe TreeIter)
findPreviousSubtree model iter = do
    prev <- treeModelIterPrevious model iter
    if isJust prev then return prev
    else maybe (return Nothing) (findPreviousSubtree model) =<< treeModelIterParent model iter

treeModelIterPrevious :: TreeModelClass treeModel => treeModel -> TreeIter -> IO (Maybe TreeIter)
treeModelIterPrevious model iter = do
    parent <- treeModelIterParent model iter
    currentPath <- treeModelGetPath model iter
    let nth = last currentPath
    if nth > 0 then treeModelIterNthChild model parent (nth-1)
    else return Nothing

treeModelIterLastChild :: TreeModelClass treeModel => treeModel -> TreeIter -> IO (Maybe TreeIter)
treeModelIterLastChild model iter = do
    childrenCount <- treeModelIterNChildren model $ Just iter
    if childrenCount > 0 then treeModelIterNthChild model (Just iter) (childrenCount - 1)
    else return Nothing
