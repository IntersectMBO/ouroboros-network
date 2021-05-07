-- |

module LedgerOnDisk.QSM.Suite where

import           Test.Tasty
import           Test.Tasty.QuickCheck
import qualified LedgerOnDisk.QSM.Model
import           LedgerOnDisk.Simple
import Test.StateMachine.Lockstep.Simple
import Data.IORef

prop_stateMachine_SimpleT :: Property
prop_stateMachine_SimpleT = ioProperty $ do
  ref <- newIORef $ initialState mempty

  let
    smt0 = LedgerOnDisk.QSM.Model.stateMachineTest $ flip runSimpleTWithIORef ref
    smt = smt0 { cleanup = \x -> cleanup smt0 x *> writeIORef ref (initialState mempty) }
  pure $ prop_sequential smt Nothing

tests :: TestTree
tests = testGroup "quickcheck state machine"
  [ testProperty "SimpleT" prop_stateMachine_SimpleT
  ]
