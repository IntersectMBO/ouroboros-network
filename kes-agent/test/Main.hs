module Main where

import Test.Tasty (defaultMain, TestTree, testGroup)
import qualified Cardano.KESAgent.Tests.Simulation as Simulation
import qualified Cardano.KESAgent.Tests.RefCounting as RefCounting
import Cardano.KESAgent.Tests.Util (Lock, newLock)
import Cardano.Crypto.Libsodium

main :: IO ()
main = do
  sodiumInit
  lock <- newLock
  defaultMain (tests lock)

tests :: Lock -> TestTree
tests lock = testGroup "KES Agent"
  [ RefCounting.tests
  , Simulation.tests lock
  ]
