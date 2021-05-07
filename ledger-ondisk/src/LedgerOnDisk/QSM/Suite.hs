{-# LANGUAGE BangPatterns #-}
-- |

module LedgerOnDisk.QSM.Suite where

import           Test.Tasty
import           Test.Tasty.QuickCheck
import qualified LedgerOnDisk.QSM.Model
import           LedgerOnDisk.Simple
import Test.StateMachine.Lockstep.Simple
import Data.IORef
import System.IO.Unsafe
import GHC.Base (noinline)


prop_stateMachine_SimpleT :: Property
prop_stateMachine_SimpleT = let
  !ref = noinline . unsafePerformIO $ newIORef $ initialState mempty
  smt0 = LedgerOnDisk.QSM.Model.stateMachineTest $ flip runSimpleTWithIORef ref
  smt = smt0 { cleanup = \x -> cleanup smt0 x *> writeIORef ref (initialState mempty) }
  in prop_sequential smt (Just 10)

tests :: TestTree
tests = testGroup "quickcheck state machine"
  [ testProperty "SimpleT" prop_stateMachine_SimpleT
  ]
