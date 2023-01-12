module Main where

import Test.Tasty (defaultMain, TestTree, testGroup)
import qualified Cardano.KESAgent.Tests.Simulation as Simulation
import qualified Cardano.KESAgent.Tests.RefCounting as RefCounting
import Test.Crypto.Util (Lock, mkLock)
import Cardano.Crypto.Libsodium

main :: IO ()
main = do
  sodiumInit
  lock <- mkLock
  defaultMain (tests lock)

tests :: Lock -> TestTree
tests lock = testGroup "KES Agent"
  [ RefCounting.tests
  , Simulation.tests lock
  ]
