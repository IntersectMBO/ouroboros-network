module Main (main) where

import           Test.Tasty

import qualified Test.Data.Monoid.Synchronisation as Synchronisation

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "monoid-synchronisation"
  [ Synchronisation.tests
  ]


