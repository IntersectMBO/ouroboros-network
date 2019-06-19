module Test.Ouroboros.Storage.ChainDB (
    tests
  ) where

import           Test.Tasty

import qualified Test.Ouroboros.Storage.ChainDB.AddBlock as AddBlock
import qualified Test.Ouroboros.Storage.ChainDB.Mock as Mock
import qualified Test.Ouroboros.Storage.ChainDB.Model as Model
import qualified Test.Ouroboros.Storage.ChainDB.StateMachine as StateMachine

tests :: TestTree
tests = testGroup "ChainDB" [
      AddBlock.tests
    , Model.tests
    , Mock.tests
    , StateMachine.tests
    ]
