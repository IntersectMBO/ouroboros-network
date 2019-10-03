module Test.Ouroboros.Storage.ChainDB (
    tests
  ) where

import           Test.Tasty

import qualified Test.Ouroboros.Storage.ChainDB.AddBlock as AddBlock
import qualified Test.Ouroboros.Storage.ChainDB.ImmDB as ImmDB
import qualified Test.Ouroboros.Storage.ChainDB.Iterator as Iterator
import qualified Test.Ouroboros.Storage.ChainDB.Mock.Test as Mock
import qualified Test.Ouroboros.Storage.ChainDB.Model.Test as Model
import qualified Test.Ouroboros.Storage.ChainDB.StateMachine as StateMachine

tests :: TestTree
tests = testGroup "ChainDB" [
      AddBlock.tests
    , ImmDB.tests
    , Iterator.tests
    , Model.tests
    , Mock.tests
    , StateMachine.tests
    ]
