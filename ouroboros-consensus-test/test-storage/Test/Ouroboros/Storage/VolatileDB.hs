module Test.Ouroboros.Storage.VolatileDB (tests) where

import           Test.Tasty (TestTree, testGroup)

import qualified Test.Ouroboros.Storage.VolatileDB.StateMachine as StateMachine


tests :: TestTree
tests = testGroup "VolatileDB"
    [ StateMachine.tests
    ]
