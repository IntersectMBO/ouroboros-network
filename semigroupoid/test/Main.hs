module Main (main) where

import           Test.Tasty

import           Test.Data.Semigroupoid (tests)



main :: IO ()
main = defaultMain $
  testGroup "Data" [
      Test.Data.Semigroupoid.tests
    ]
