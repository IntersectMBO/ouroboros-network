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
  , TestConfig (..)
  , VTime
  , allEqual
  , nodeStake
  , numNodes
  , k
  , kPerEpoch
  , numEpochs
  , numSlots
  , shortestLength
  ) where

import           Control.Monad.ST.Lazy (runST)
import           Data.Foldable (foldl')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Numeric.Natural (Natural)
import           Test.QuickCheck

import           Ouroboros.Network.Block
import           Ouroboros.Network.Chain
import qualified Ouroboros.Network.Chain as Chain
import           Ouroboros.Network.MonadClass
import           Ouroboros.Network.Sim (VTime)

import           Ouroboros.Consensus.Demo
import           Ouroboros.Consensus.Ledger.Mock
import           Ouroboros.Consensus.Node
import qualified Ouroboros.Consensus.Util.Chain as Chain
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.Random

import           Test.Dynamic.Network

data TestConfig = TestConfig {
      testAddressDistribution :: Map Addr NodeId
      -- ^ Some form of mapping that allows us to partition incoming
      -- transactions in a way that we can accumulate the stake properly.
    }

ourAddr :: TestConfig -> NodeId -> Addr -> Bool
ourAddr TestConfig{..} myNodeId address =
    fmap ((==) myNodeId) (Map.lookup address testAddressDistribution)
        == Just True

nodeStake :: TestConfig -> Utxo -> NodeId -> Int
nodeStake cfg u nodeId =
    Map.foldl
        (\acc (a, stake) -> if ourAddr cfg nodeId a then acc + stake else acc)
        0
        u

numNodes, numEpochs, k, kPerEpoch, numSlots :: Int
numNodes  = 3
numEpochs = 4
k         = 5
kPerEpoch = 3
numSlots  = k * kPerEpoch * numEpochs

prop_simple_protocol_convergence :: forall p. DemoProtocolConstraints p
                                 => (CoreNodeId -> ProtocolInfo p)
                                 -> (   [NodeId]
                                     -> [(VTime, Map NodeId (Chain (Block p)))]
                                     -> Property)
                                 -> Seed
                                 -> Property
prop_simple_protocol_convergence pInfo isValid seed =
    runST $ test_simple_protocol_convergence pInfo isValid seed

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
                                 -> Seed
                                 -> n Property
test_simple_protocol_convergence pInfo isValid seed = do
    fmap (isValid nodeIds) $ withProbe $ go
  where
    go :: Probe m (Map NodeId (Chain (Block p))) -> m ()
    go p = do
      btime <- testBlockchainTime numSlots 100000
      finalChains <- broadcastNetwork
                       btime
                       (NumCoreNodes numNodes)
                       pInfo
                       (seedToChaCha seed)
                       numSlots
      probeOutput p finalChains

    nodeIds :: [NodeId]
    nodeIds = [CoreId n | n <- [0 .. numNodes - 1]]

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

allEqual :: forall b. (Condense b, Eq b, HasHeader b) => [Chain b] -> Property
allEqual []             = property True
allEqual [_]            = property True
allEqual (x : xs@(_:_)) =
    let c = foldl' Chain.commonPrefix x xs
    in  foldl' (\prop d -> prop .&&. f c d) (property True) xs
  where
    f :: Chain b -> Chain b -> Property
    f c d = counterexample (g c d) $ c == d

    g :: Chain b -> Chain b -> String
    g c d = case (Chain.lastSlot c, Chain.lastSlot d) of
        (Nothing, Nothing) -> error "impossible case"
        (Nothing, Just t)  ->    "empty intersection of non-empty chains (one reaches slot "
                              <> show (getSlot t)
                              <> " and contains "
                              <> show (Chain.length d)
                              <> "blocks): "
                              <> condense d
        (Just _, Nothing)  -> error "impossible case"
        (Just s, Just t)   ->    "intersection reaches slot "
                              <> show (getSlot s)
                              <> " and has length "
                              <> show (Chain.length c)
                              <> ", but at least one chain reaches slot "
                              <> show (getSlot t)
                              <> " and has length "
                              <> show (Chain.length d)
                              <> ": "
                              <> condense c
                              <> " /= "
                              <> condense d

shortestLength :: Map NodeId (Chain b) -> Natural
shortestLength = fromIntegral . minimum . map Chain.length . Map.elems
