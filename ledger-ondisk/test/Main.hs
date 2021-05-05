module Main (main) where

import           Test.Tasty
import           Test.Tasty.QuickCheck
import qualified Lib

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "ledger-ondisk"
  [ testProperty "foo" Lib.foo ]
