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
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS -fno-warn-unused-binds #-}
{-# OPTIONS -fno-warn-orphans #-}

module Test.Dynamic.Praos (
    tests
  ) where

import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Test.QuickCheck

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Network.Block
import           Ouroboros.Network.Chain
import qualified Ouroboros.Network.Chain as Chain
import           Ouroboros.Network.Node

import           Ouroboros.Consensus.Crypto.KES (VerKeyKES)
import           Ouroboros.Consensus.Crypto.KES.Mock
import           Ouroboros.Consensus.Crypto.VRF (VerKeyVRF)
import           Ouroboros.Consensus.Crypto.VRF.Mock
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Mock
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.ExtNodeConfig
import           Ouroboros.Consensus.Protocol.Praos
import           Ouroboros.Consensus.Util (Condense (..))
import           Ouroboros.Consensus.Util.Chain (dropLastBlocks, lastSlot)
import           Ouroboros.Consensus.Util.Random

import           Test.Dynamic.General

tests :: TestTree
tests = testGroup "Dynamic chain generation" [
      testProperty "simple Praos convergence" prop_simple_praos_convergence
    ]

type Protocol = Praos PraosMockCrypto

instance HasPayload (Praos PraosMockCrypto) (BlockUnderTest Protocol) where
  blockPayload _ = testPayloadP
                 . encPayloadP
                 . headerOuroboros
                 . simpleHeader

instance ProtocolLedgerView (BlockUnderTest Protocol) where
    protocolLedgerView (EncNodeConfig _ cfg) (SimpleLedgerState u) =
        ( relativeStakes $ totalStakes cfg u
        , nodeStake cfg u
        )

prop_simple_praos_convergence :: Seed -> Property
prop_simple_praos_convergence =
    prop_simple_protocol_convergence mkConfig mkState initialChainState isValid
  where
    mkConfig :: Int -> NodeConfig Protocol
    mkConfig i = PraosNodeConfig
        { praosNodeId        = CoreId i
        , praosSignKeyVRF    = SignKeyMockVRF i
        , praosSlotsPerEpoch = fromIntegral $ k * kPerEpoch
        , praosInitialEta    = 0
        , praosInitialStake  = initialStake
        , praosLeaderF       = 0.5
        , praosK             = fromIntegral k
        , praosVerKeys       = verKeys
        }

    mkState :: Int -> NodeState Protocol
    mkState nid = SignKeyMockKES ( fst $ verKeys IntMap.! nid
                                 , 0
                                 , 1 + fromIntegral numSlots
                                 )

    verKeys :: IntMap (VerKeyKES MockKES, VerKeyVRF MockVRF)
    verKeys = IntMap.fromList [ (nid, (VerKeyMockKES nid, VerKeyMockVRF nid))
                              | nid <- [0 .. numNodes - 1]]

    initialStake :: StakeDist
    initialStake =
        let q = recip $ fromIntegral numNodes
        in  IntMap.fromList [(i, q) | i <- [0 .. numNodes - 1]]

    initialChainState :: ChainState Protocol
    initialChainState = []

    isValid :: [NodeId]
            -> [(VTime, Map NodeId (Chain (BlockUnderTest Protocol)))]
            -> Property
    isValid nodeIds trace = counterexample (show trace) $
      case trace of
        [(_, final)] ->   collect (shortestLength final)
                     $    Map.keys final == nodeIds
                     .&&. prop_all_common_prefix k (Map.elems final)
        _otherwise   -> property False

prop_common_prefix :: forall b. (HasHeader b, Condense b, Eq b)
                   => Int -> Chain b -> Chain b -> Property
prop_common_prefix l x y = go x y .&&. go y x
  where
    go c d =
        let c' = dropLastBlocks l c
            e  = "after dropping "
                 <> show l
                 <> " blocks from "
                 <> showChain c
                 <> ", the resulting "
                 <> showChain c'
                 <> " is not a prefix of "
                 <> showChain d
        in  counterexample "" $ c' `isPrefixOf` d

    showChain :: Chain b -> String
    showChain c = condense c
                  <> "(length "
                  <> show (Chain.length c)
                  <> case lastSlot c of
                        Nothing -> ")"
                        Just s  ->    ", last slot "
                                   <> show (getSlot s)
                                   <> ")"

prop_all_common_prefix :: (HasHeader b, Condense b, Eq b)
                       => Int -> [Chain b] -> Property
prop_all_common_prefix _ []     = property True
prop_all_common_prefix l (c:cs) = conjoin [prop_common_prefix l c d | d <- cs]

totalStakes :: TestConfig -> Utxo -> Map (Maybe Int) Int
totalStakes (TestConfig addrDist) = foldl f Map.empty
  where
    f :: Map (Maybe Int) Int -> TxOut -> Map (Maybe Int) Int
    f m (a, stake) = case Map.lookup a addrDist of
        Just (CoreId nid) -> Map.insertWith (+) (Just nid) stake m
        _                 -> Map.insertWith (+) Nothing stake m

relativeStakes :: Map (Maybe Int) Int -> StakeDist
relativeStakes m =
    let totalStake    = fromIntegral $ sum $ Map.elems m
    in  IntMap.fromList [ (nid, fromIntegral stake / totalStake)
                        | (Just nid, stake) <- Map.toList m
                        ]
