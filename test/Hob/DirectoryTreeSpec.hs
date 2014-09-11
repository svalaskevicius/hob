module Hob.DirectoryTreeSpec (main, spec) where

import Control.Monad.Error
import Data.Tree
import Hob.DirectoryTree
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "fileTreeGenerator" $ do
    it "returns directory contents" $ do
        let dirContentsStub _ = return ["nd1", "nd2"]
            isDirStub _ = return False
            fileTreeGenerator' = fileTreeGenerator dirContentsStub isDirStub
        items <- fileTreeGenerator' "/xxx/"
        items `shouldBe` [Node (DirectoryTreeElement "nd1" "/xxx/nd1" False) [],
                          Node (DirectoryTreeElement "nd2" "/xxx/nd2" False) []]

    it "returns sub-directory contents" $ do
        let dirContentsStub "/xxx" = return ["d1"]
            dirContentsStub "/xxx/d1" = return ["nd2"]
            dirContentsStub _ = throwError $ userError "unexpected directory listing"
            isDirStub d = return $ "/xxx/d1" == d
            fileTreeGenerator' = fileTreeGenerator dirContentsStub isDirStub
        items <- fileTreeGenerator' "/xxx"
        items `shouldBe` [Node (DirectoryTreeElement "d1" "/xxx/d1" True) [
                          Node (DirectoryTreeElement "nd2" "/xxx/d1/nd2" False) []]]

    it "hides dot (. | ..) directory contents" $ do
        let dirContentsStub "/xxx" = return ["d1", ".", ".."]
            dirContentsStub "/xxx/d1" = return ["nd2", ".", ".."]
            dirContentsStub _ = throwError $ userError "unexpected directory listing"
            isDirStub d = return $ "/xxx/d1" == d
            fileTreeGenerator' = fileTreeGenerator dirContentsStub isDirStub
        items <- fileTreeGenerator' "/xxx"
        items `shouldBe` [Node (DirectoryTreeElement "d1" "/xxx/d1" True) [
                          Node (DirectoryTreeElement "nd2" "/xxx/d1/nd2" False) []]]


    it "hides .git directories" $ do
        let dirContentsStub "/xxx" = return ["d1", ".git"]
            dirContentsStub "/xxx/d1" = return ["nd2"]
            dirContentsStub _ = throwError $ userError "unexpected directory listing"
            isDirStub d = return $ "/xxx/d1" == d
            fileTreeGenerator' = fileTreeGenerator dirContentsStub isDirStub
        items <- fileTreeGenerator' "/xxx"
        items `shouldBe` [Node (DirectoryTreeElement "d1" "/xxx/d1" True) [
                          Node (DirectoryTreeElement "nd2" "/xxx/d1/nd2" False) []]]

    it "sorts the directory files" $ do
        let dirContentsStub _ = return ["nd2", "nd1"]
            isDirStub _ = return False
            fileTreeGenerator' = fileTreeGenerator dirContentsStub isDirStub
        items <- fileTreeGenerator' "/xxx/"
        items `shouldBe` [Node (DirectoryTreeElement "nd1" "/xxx/nd1" False) [],
                          Node (DirectoryTreeElement "nd2" "/xxx/nd2" False) []]

    it "sorts the directory subdirectories at the top" $ do
        let dirContentsStub "/xxx/" = return ["nd1", "zd2", "zd1"]
            dirContentsStub _ = return []
            isDirStub "/xxx/zd1" = return True
            isDirStub "/xxx/zd2" = return True
            isDirStub _ = return False
            fileTreeGenerator' = fileTreeGenerator dirContentsStub isDirStub
        items <- fileTreeGenerator' "/xxx/"
        items `shouldBe` [Node (DirectoryTreeElement "zd1" "/xxx/zd1" True) [],
                          Node (DirectoryTreeElement "zd2" "/xxx/zd2" True) [],
                          Node (DirectoryTreeElement "nd1" "/xxx/nd1" False) []]

