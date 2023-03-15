module Main (main) where

import           Test.Tasty

import qualified Test.Ouroboros.Storage
import           Test.Util.TestEnv

main :: IO ()
main = defaultMainWithTestEnv defaultTestEnvConfig tests

tests :: TestTree
tests = testGroup "ouroboros-storage" [
    Test.Ouroboros.Storage.tests
  ]
