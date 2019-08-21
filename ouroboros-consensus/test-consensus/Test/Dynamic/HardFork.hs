{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE NamedFieldPuns          #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Test.Dynamic.HardFork
  ( tests
  )
where

import           Test.QuickCheck

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Demo
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Protocol
import           Ouroboros.Consensus.Util.Random

import           Test.Dynamic.General
import           Test.Dynamic.Util

import           Test.Util.Orphans.Arbitrary ()

tests :: TestTree
tests =
  testGroup "Dynamic chain generation"
    [ testProperty "simple PBFT/Praos Hard Fork convergence" testPraos -- TODO: Include pre-fork test
    ]
  where
    testPraos :: Seed -> Property
    testPraos = prop_simple_hard_fork_convergence
                  praosSecurityParam
                  (NumCoreNodes 3)
                  (NumSlots (fromIntegral numSlots))

    PraosParams{praosSecurityParam, praosSlotsPerEpoch} = defaultDemoPraosParams
    numSlots  = maxRollbacks praosSecurityParam * praosSlotsPerEpoch * numEpochs
    numEpochs = 3

prop_simple_hard_fork_convergence
  :: SecurityParam
  -> NumCoreNodes
  -> NumSlots
  -> Seed
  -> Property
prop_simple_hard_fork_convergence k numCoreNodes numSlots seed =
  counterexample (tracesToDot testOutputNodes) $
  label lbl $
  counterexample lbl $
  prop_general k schedule testOutput
  where
    testOutput@TestOutput{testOutputNodes} =
      runTestNetwork
        ( \nid ->
          protocolInfo
            numCoreNodes
            nid
            (ProtocolMockHardFork defaultDemoPBftParams defaultDemoPraosParams)
        )
        numCoreNodes
        numSlots
        seed

    schedule = leaderScheduleFromTrace numSlots testOutputNodes
    lbl = if tooCrowded k schedule then "too crowded" else "not too crowded"
