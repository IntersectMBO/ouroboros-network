module Main (main) where

import           Test.Tasty

import qualified Test.MonadTimer

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "io-classes"
    [ Test.MonadTimer.tests
    ]

