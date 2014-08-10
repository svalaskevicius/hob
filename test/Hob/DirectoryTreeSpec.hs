module Hob.DirectoryTreeSpec where

import Control.Monad.Error
import Data.Tree
import Hob.DirectoryTree
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "directory tree element" $
    it "can be deconstructed" $ do
        let el = DirectoryTreeElement "label" "path" $ IsDirectory False
        directoryTreeElementLabel el `shouldBe` "label"
        directoryTreeElementPath el `shouldBe` "path"
        directoryTreeElementIsDirectory el `shouldBe` False

  describe "fileTreeGenerator" $ do
    it "returns directory contents" $ do
        let dirContentsStub _ = return ["nd1", "nd2"]
            isDirStub _ = return False
            fileTreeGenerator' = fileTreeGenerator dirContentsStub isDirStub
        items <- fileTreeGenerator' "/xxx/"
        items `shouldBe` [Node (DirectoryTreeElement "nd1" "/xxx/nd1" (IsDirectory False)) [],
                          Node (DirectoryTreeElement "nd2" "/xxx/nd2" (IsDirectory False)) []]

    it "returns sub-directory contents" $ do
        let dirContentsStub p = case p of
                                        "/xxx" -> return ["d1"]
                                        "/xxx/d1" -> return ["nd2"]
                                        _ -> throwError $ userError "unexpected directory listing"
            isDirStub d = return $ "/xxx/d1" == d
            fileTreeGenerator' = fileTreeGenerator dirContentsStub isDirStub
        items <- fileTreeGenerator' "/xxx"
        items `shouldBe` [Node (DirectoryTreeElement "d1" "/xxx/d1" (IsDirectory True)) [
                          Node (DirectoryTreeElement "nd2" "/xxx/d1/nd2" (IsDirectory False)) []]]

    it "returns hides dot (. | ..) directory contents" $ do
        let dirContentsStub p = case p of
                                        "/xxx" -> return ["d1", ".", ".."]
                                        "/xxx/d1" -> return ["nd2", ".", ".."]
                                        _ -> throwError $ userError "unexpected directory listing"
            isDirStub d = return $ "/xxx/d1" == d
            fileTreeGenerator' = fileTreeGenerator dirContentsStub isDirStub
        items <- fileTreeGenerator' "/xxx"
        items `shouldBe` [Node (DirectoryTreeElement "d1" "/xxx/d1" (IsDirectory True)) [
                          Node (DirectoryTreeElement "nd2" "/xxx/d1/nd2" (IsDirectory False)) []]]


