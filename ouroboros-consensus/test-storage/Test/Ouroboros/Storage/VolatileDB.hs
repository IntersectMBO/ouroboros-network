module Test.Ouroboros.Storage.VolatileDB (tests) where

import           GHC.Stack (HasCallStack)

import           Test.Tasty (TestTree, testGroup)

import qualified Test.Ouroboros.Storage.VolatileDB.StateMachine as StateMachine


tests :: HasCallStack => TestTree
tests = testGroup "VolatileDB"
    [ StateMachine.tests
    ]
