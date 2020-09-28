module Main (main) where

import           Test.Tasty

import qualified Test.Test.QuickCheck.Randomized.Coin as Coin

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "Test.QuickCheck.Randomized"
  [ Coin.tests
  ]
