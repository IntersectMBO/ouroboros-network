{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS -fno-warn-orphans #-}

module Test.Dynamic.PBFT (
    tests
  ) where

import qualified Data.Map.Strict as Map
import           Test.QuickCheck

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Demo
import           Ouroboros.Consensus.Ledger.Mock
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol
import           Ouroboros.Consensus.Util.Random
import           Ouroboros.Network.MockChain.Chain (Chain)

import           Test.Dynamic.General
import           Test.Dynamic.Util

import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Range

tests :: TestTree
tests = testGroup "Dynamic chain generation" [
      testProperty "simple PBFT convergence" $
        prop_simple_pbft_convergence sp
    ]
  where
    sp = defaultSecurityParam

prop_simple_pbft_convergence :: SecurityParam
                             -> NumCoreNodes
                             -> NumSlots
                             -> Seed
                             -> Property
prop_simple_pbft_convergence sp numCoreNodes@(NumCoreNodes nn) =
    prop_simple_protocol_convergence
      (\nid -> protocolInfo numCoreNodes nid (ProtocolMockPBFT params))
      isValid
      numCoreNodes
  where
    sigWin = fromIntegral $ nn * 10
    sigThd = (1.0 / fromIntegral nn) + 0.1
    params = PBftParams sp (fromIntegral nn) sigWin sigThd
    isValid :: [NodeId]
            -> TestOutput (SimplePBftBlock SimpleMockCrypto PBftMockCrypto)
            -> Property
    isValid nodeIds TestOutput{testOutputNodes = final} =
          counterexample (show final')
     $    tabulate "shortestLength" [show (rangeK sp (shortestLength final'))]
     $    Map.keys final === nodeIds
     .&&. allEqual (takeChainPrefix <$> Map.elems final')
      where
        -- Without the 'NodeConfig's
        final' = snd <$> final
        takeChainPrefix :: Chain (SimplePBftBlock SimpleMockCrypto PBftMockCrypto)
                        -> Chain (SimplePBftBlock SimpleMockCrypto PBftMockCrypto)
        takeChainPrefix = id -- in PBFT, chains should indeed all be equal.
