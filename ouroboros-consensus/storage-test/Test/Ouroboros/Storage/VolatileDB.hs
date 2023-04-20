module Test.Ouroboros.Storage.VolatileDB (tests) where

import qualified Test.Ouroboros.Storage.VolatileDB.StateMachine as StateMachine
import           Test.Tasty (TestTree, testGroup)


tests :: TestTree
tests = testGroup "VolatileDB"
    [ StateMachine.tests
    ]
