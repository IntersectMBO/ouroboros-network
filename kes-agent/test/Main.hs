module Main where

import Test.Tasty (defaultMain, TestTree, testGroup)
import qualified Cardano.KESAgent.Tests.Simulation as Simulation
import qualified Cardano.KESAgent.Tests.RefCounting as RefCounting
import Cardano.Crypto.Libsodium
import Ouroboros.Network.Snocket
import System.IOManager
import Network.Socket
import Control.Tracer (nullTracer)

main :: IO ()
main = withIOManager $ \ioManager -> do
  sodiumInit
  lock <- Simulation.mkLock
  defaultMain (tests lock ioManager)

tests :: Simulation.Lock IO -> IOManager -> TestTree
tests lock ioManager = testGroup "KES Agent"
  [ RefCounting.tests
  , Simulation.tests lock nullTracer ioManager
  ]
