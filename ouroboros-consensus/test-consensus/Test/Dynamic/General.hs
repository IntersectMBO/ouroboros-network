{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

module Test.Dynamic.General (
    prop_simple_protocol_convergence
  , VTime
  ) where

import           Control.Monad.ST.Lazy (runST)
import           Data.Map.Strict (Map)
import           Test.QuickCheck

import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTimer
import           Control.Monad.Class.MonadProbe
import           Control.Monad.IOSim (VTime)

import           Ouroboros.Network.Chain

import           Ouroboros.Consensus.Demo
import           Ouroboros.Consensus.Node
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.Random

import           Test.Dynamic.Network

prop_simple_protocol_convergence :: forall p. DemoProtocolConstraints p
                                 => (CoreNodeId -> ProtocolInfo p)
                                 -> (   [NodeId]
                                     -> [(VTime, Map NodeId (Chain (Block p)))]
                                     -> Property)
                                 -> NumSlots
                                 -> NumCoreNodes
                                 -> Seed
                                 -> Property
prop_simple_protocol_convergence pInfo isValid numSlots numCoreNodes seed =
    runST $ test_simple_protocol_convergence pInfo isValid numSlots numCoreNodes seed

-- Run protocol on the broadcast network, and check resulting chains on all nodes.
test_simple_protocol_convergence :: forall m n p.
                                    ( MonadSTM m
                                    , MonadRunProbe m n
                                    , MonadSay m
                                    , MonadTimer m
                                    , DemoProtocolConstraints p
                                    )
                                 => (CoreNodeId -> ProtocolInfo p)
                                 -> (   [NodeId]
                                     -> [(Time m, Map NodeId (Chain (Block p)))]
                                     -> Property)
                                 -> NumSlots
                                 -> NumCoreNodes
                                 -> Seed
                                 -> n Property
test_simple_protocol_convergence pInfo isValid numSlots numCoreNodes seed = do
    fmap (isValid nodeIds) $ withProbe $ go
  where
    go :: Probe m (Map NodeId (Chain (Block p))) -> m ()
    go p = do
      btime <- testBlockchainTime numSlots 100000
      finalChains <- broadcastNetwork
                       btime
                       numCoreNodes
                       pInfo
                       (seedToChaCha seed)
                       numSlots
      probeOutput p finalChains

    nodeIds :: [NodeId]
    nodeIds = map fromCoreNodeId $ enumCoreNodes numCoreNodes
