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
  let snocket = socketSnocket ioManager
  defaultMain (tests lock snocket)

tests :: Simulation.Lock IO -> Snocket IO Socket SockAddr -> TestTree
tests lock snocket = testGroup "KES Agent"
  [ RefCounting.tests
  , Simulation.tests lock nullTracer snocket
  ]
