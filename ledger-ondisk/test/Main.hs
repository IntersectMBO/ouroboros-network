module Main (main) where

import           Test.Tasty
import           Test.Tasty.QuickCheck

import qualified LedgerOnDisk

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "ledger-ondisk"
  [ testProperty "foo" LedgerOnDisk.foo ]
  -- TODO simple properties for simple implemetnation, i.e. insert, delete
