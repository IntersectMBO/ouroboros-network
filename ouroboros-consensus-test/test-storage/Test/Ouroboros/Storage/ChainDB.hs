module Test.Ouroboros.Storage.ChainDB (tests) where

import           System.Info (os)

import           Test.Tasty

import qualified Test.Ouroboros.Storage.ChainDB.GcSchedule as GcSchedule
import qualified Test.Ouroboros.Storage.ChainDB.Iterator as Iterator
import qualified Test.Ouroboros.Storage.ChainDB.Model.Test as Model
import qualified Test.Ouroboros.Storage.ChainDB.Paths as Paths
import qualified Test.Ouroboros.Storage.ChainDB.StateMachine as StateMachine

tests :: TestTree
tests = testGroup "ChainDB" $ [
      Iterator.tests
    , GcSchedule.tests
    , Model.tests
    , Paths.tests
    ] <>
    -- The ChainDB q-s-m test is flaky on Windows, see
    -- https://github.com/input-output-hk/ouroboros-network/issues/3874
    [ StateMachine.tests | os /= "mingw32" ]
