module Hob.ControlSpec (main, spec) where

import Data.IORef

import Test.Hspec

import Hob.Control

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "maybeDo" $ do
    it "does not execute on Nothing" $ do
        (executor, mockReader) <- mockedExecutor
        maybeDo executor Nothing
        executionStatus <- mockReader
        executionStatus `shouldBe` Nothing

    it "passes unwrapped value on Just" $ do
        (executor, mockReader) <- mockedExecutor
        maybeDo executor $ Just "test value"
        executionStatus <- mockReader
        executionStatus `shouldBe` Just "test value"

mockedExecutor :: IO (String -> IO(), IO (Maybe String))
mockedExecutor = do
    recorder <- newIORef Nothing
    return (writeIORef recorder . Just, readIORef recorder)
