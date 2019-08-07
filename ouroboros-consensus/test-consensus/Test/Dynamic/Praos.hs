{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS -fno-warn-orphans #-}

module Test.Dynamic.Praos (
    tests
  ) where

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
tests = testGroup "Dynamic chain generation"
    [ testProperty "simple Praos convergence - special case (issue #131)" $
        testPraos $ Seed ( 49644418094676
                         , 40315957626756
                         , 42668365444963
                         , 9796082466547
                         , 32684299622558
                         )
    , testProperty "simple Praos convergence - special crowded case" $
        testPraos $ Seed ( 8871923881324151440
                         , 881094692332313449
                         , 3091285302407489889
                         , 6410351877547894330
                         , 14676014321459888687
                         )
    , testProperty "simple Praos convergence" testPraos
    ]
  where
    testPraos :: Seed -> Property
    testPraos = prop_simple_praos_convergence
                  params
                  (NumCoreNodes 3)
                  (NumSlots (fromIntegral numSlots))

    params@PraosParams{..} = defaultDemoPraosParams
    numSlots  = maxRollbacks praosSecurityParam * praosSlotsPerEpoch * numEpochs
    numEpochs = 3

prop_simple_praos_convergence :: PraosParams
                              -> NumCoreNodes
                              -> NumSlots
                              -> Seed
                              -> Property
prop_simple_praos_convergence
  params@PraosParams{praosSecurityParam = k} numCoreNodes numSlots seed =
    counterexample (tracesToDot testOutputNodes) $
    if tooCrowded k schedule
      then label "too crowded"     $ property True
      else label "not too crowded" $
               prop_general k schedule testOutput
  where
    testOutput@TestOutput{testOutputNodes} =
        runTestNetwork
            (\nid -> protocolInfo numCoreNodes nid (ProtocolMockPraos params))
            numCoreNodes numSlots seed

    schedule = leaderScheduleFromTrace numSlots testOutputNodes
