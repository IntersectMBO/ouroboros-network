module Test.Ouroboros.Storage.ChainDB (
    tests
  ) where

import           Test.Tasty

import qualified Test.Ouroboros.Storage.ChainDB.GcSchedule as GcSchedule
import qualified Test.Ouroboros.Storage.ChainDB.Iterator as Iterator
import qualified Test.Ouroboros.Storage.ChainDB.Model.Test as Model
import qualified Test.Ouroboros.Storage.ChainDB.StateMachine as StateMachine
import qualified Test.Ouroboros.Storage.ChainDB.VolDB as VolDB

tests :: TestTree
tests = testGroup "ChainDB" [
      Iterator.tests
    , GcSchedule.tests
    , Model.tests
    , StateMachine.tests
    , VolDB.tests
    ]
