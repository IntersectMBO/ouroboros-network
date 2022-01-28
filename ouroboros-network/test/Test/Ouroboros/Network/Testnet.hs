{-# LANGUAGE RankNTypes #-}
module Test.Ouroboros.Network.Testnet (tests) where

import           Control.Monad.IOSim

import           Data.List (intercalate)
import           Data.Void (Void)

import           Ouroboros.Network.Testing.Data.AbsBearerInfo
                     (AbsBearerInfo (..), attenuation, delay, toSduSize)
import           Simulation.Network.Snocket (BearerInfo (..))

import           Test.Ouroboros.Network.Testnet.Simulation.Node
                     (DiffusionScript, diffusion_simulation,
                     prop_diffusionScript_commandScript_valid,
                     prop_diffusionScript_fixupCommands)

import           Test.QuickCheck (Property, counterexample)
import           Test.Tasty
import           Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup "Ouroboros.Network.Testnet"
  [ testGroup "multinodeSim"
    [ testProperty "diffusionScript fixupCommands idempotent"
                   prop_diffusionScript_fixupCommands
    , testProperty "diffusionScript command script valid"
                   prop_diffusionScript_commandScript_valid
    , testProperty "test"
                   test
    ]
  ]

test :: AbsBearerInfo
     -> DiffusionScript
     -> Property
test defaultBearerInfo dmnScript =
    let trace = traceEvents
              $ runSimTrace sim
     in counterexample (intercalate "\n"
                       $ map show
                       $ take 1000 trace)
                       True
  where
    sim :: forall s . IOSim s Void
    sim = diffusion_simulation (toBearerInfo defaultBearerInfo) dmnScript

toBearerInfo :: AbsBearerInfo -> BearerInfo
toBearerInfo abi =
    BearerInfo {
        biConnectionDelay      = delay (abiConnectionDelay abi),
        biInboundAttenuation   = attenuation (abiInboundAttenuation abi),
        biOutboundAttenuation  = attenuation (abiOutboundAttenuation abi),
        biInboundWriteFailure  = abiInboundWriteFailure abi,
        biOutboundWriteFailure = abiOutboundWriteFailure abi,
        biAcceptFailures       = Nothing, -- TODO
        biSDUSize              = toSduSize (abiSDUSize abi)
      }



