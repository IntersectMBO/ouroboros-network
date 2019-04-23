module Test.Ouroboros.Storage.ChainDB (
    tests
  ) where

import           Test.Tasty

import qualified Test.Ouroboros.Storage.ChainDB.Mock as Mock
import qualified Test.Ouroboros.Storage.ChainDB.Model as Model

tests :: TestTree
tests = testGroup "ChainDB" [
      Model.tests
    , Mock.tests
    ]
