{-# LANGUAGE ImportQualifiedPost #-}

module Main
where

import Cardano.KESAgent.Tests.EndToEnd qualified as EndToEnd
import Cardano.KESAgent.Tests.OCert qualified as OCert
import Cardano.KESAgent.Tests.RefCounting qualified as RefCounting
import Cardano.KESAgent.Tests.Serialization qualified as Serialization
import Cardano.KESAgent.Tests.Simulation qualified as Simulation

import Cardano.Crypto.Libsodium

import Control.Tracer (nullTracer)
import System.IOManager
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = withIOManager $ \ioManager -> do
  sodiumInit
  lock <- Simulation.mkLock
  defaultMain (tests lock ioManager)

tests :: Simulation.Lock IO -> IOManager -> TestTree
tests lock ioManager =
  testGroup
    "KES Agent"
    [ RefCounting.tests
    , OCert.tests
    , Serialization.tests
    , Simulation.tests lock nullTracer ioManager
    , EndToEnd.tests
    ]
