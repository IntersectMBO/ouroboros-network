{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}

module Test.Dynamic.General (
    prop_simple_protocol_convergence
  ) where

import           Data.Map.Strict (Map)
import           Test.QuickCheck

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork (MonadFork)
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Monad.IOSim (runSimOrThrow)

import           Ouroboros.Network.Chain

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Demo
import           Ouroboros.Consensus.Ledger.Mock
import           Ouroboros.Consensus.Node
import           Ouroboros.Consensus.Protocol.Abstract (NodeConfig)
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.Random
import           Ouroboros.Consensus.Util.ThreadRegistry

import           Test.Dynamic.Network

prop_simple_protocol_convergence :: forall p c.
                                   ( RunDemo (SimpleBlock p c) (SimpleHeader p c)
                                   , SimpleBlockCrypto c
                                   )
                                 => (CoreNodeId -> ProtocolInfo (SimpleBlock p c))
                                 -> (   [NodeId]
                                     -> Map NodeId (NodeConfig p, Chain (SimpleBlock p c))
                                     -> Property)
                                 -> NumCoreNodes
                                 -> NumSlots
                                 -> Seed
                                 -> Property
prop_simple_protocol_convergence pInfo isValid numCoreNodes numSlots seed =
    runSimOrThrow $
      test_simple_protocol_convergence pInfo isValid numCoreNodes numSlots seed

-- Run protocol on the broadcast network, and check resulting chains on all nodes.
test_simple_protocol_convergence :: forall m p c.
                                    ( MonadAsync m
                                    , MonadFork  m
                                    , MonadMask  m
                                    , MonadSay   m
                                    , MonadTime  m
                                    , MonadTimer m
                                    , MonadThrow (STM m)
                                    , RunDemo (SimpleBlock p c) (SimpleHeader p c)
                                    , SimpleBlockCrypto c
                                    )
                                 => (CoreNodeId -> ProtocolInfo (SimpleBlock p c))
                                 -> (   [NodeId]
                                     -> Map NodeId (NodeConfig p, Chain (SimpleBlock p c))
                                     -> Property)
                                 -> NumCoreNodes
                                 -> NumSlots
                                 -> Seed
                                 -> m Property
test_simple_protocol_convergence pInfo isValid numCoreNodes numSlots seed =
    fmap (isValid nodeIds) $ withThreadRegistry $ \registry -> do
      btime <- testBlockchainTime registry numSlots 100000
      broadcastNetwork registry
                       btime
                       numCoreNodes
                       pInfo
                       (seedToChaCha seed)
                       numSlots
  where
    nodeIds :: [NodeId]
    nodeIds = map fromCoreNodeId $ enumCoreNodes numCoreNodes
