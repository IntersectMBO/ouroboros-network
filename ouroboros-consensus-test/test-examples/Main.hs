module Main (main) where

-- pkgs tasty*:
import           Test.Tasty
import           Test.Tasty.QuickCheck

-- local modules:
import           Test.Tutorial
import           Test.Tutorial2
import           Test.Example
import           Test.ToyLedgerC
import           Test.ToyLedgerD
import           Test.ToyLedger1 -- defunct?

main :: IO ()
main = defaultMain test

-- FIXME: add real [quickcheck] tests, ideally in separate modules.

test :: TestTree
test =
  testGroup "ouroboros-examples"
  [ testProperty "prop_example1" prop_example1
  ]
  where
  prop_example1 :: Bool
  prop_example1 = True
  

