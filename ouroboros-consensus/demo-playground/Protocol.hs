{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Protocol (
    DemoProtocol(..)
  , DemoBFT
  , DemoPraos
  , Block
  , ProtocolInfo(..)
  , DemoProtocolConstraints
  , demoProtocolConstraints
  , protocolInfo
  ) where

import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Ouroboros.Network.Serialise (Serialise)

import           Ouroboros.Consensus.Crypto.DSIGN
import           Ouroboros.Consensus.Crypto.KES
import           Ouroboros.Consensus.Crypto.VRF
import           Ouroboros.Consensus.Ledger.Abstract
import qualified Ouroboros.Consensus.Ledger.Mock as Mock
import           Ouroboros.Consensus.Node (NodeId (..))
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.BFT
import           Ouroboros.Consensus.Protocol.ExtNodeConfig
import           Ouroboros.Consensus.Protocol.Praos
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.Condense

type DemoBFT   = Bft BftMockCrypto
type DemoPraos = ExtNodeConfig
                   (Map Mock.Addr NodeId)
                   (Praos PraosMockCrypto)

-- | Consensus protocol to use
data DemoProtocol p where
  DemoBFT   :: DemoProtocol DemoBFT
  DemoPraos :: DemoProtocol DemoPraos

-- | Our 'Block' type stays the same.
type Block p = Mock.SimpleBlock p Mock.SimpleBlockMockCrypto

-- | Data required to run the specified protocol
data ProtocolInfo p = ProtocolInfo {
        initLedger     :: ExtLedgerState (Block p)
      , protocolConfig :: NodeConfig p
      , nodeState      :: NodeState p
      }

type DemoProtocolConstraints p = (
    ProtocolLedgerView (Block p)
  , Condense  (Payload p (Mock.SimplePreHeader p Mock.SimpleBlockMockCrypto))
  , Eq        (Payload p (Mock.SimplePreHeader p Mock.SimpleBlockMockCrypto))
  , Serialise (Payload p (Mock.SimplePreHeader p Mock.SimpleBlockMockCrypto))
  )

demoProtocolConstraints :: DemoProtocol p -> Dict (DemoProtocolConstraints p)
demoProtocolConstraints DemoBFT   = Dict
demoProtocolConstraints DemoPraos = Dict

-- | Info needed to run the selected protocol
--
-- TODO: There is overlap between this code and the consensus tests, which
-- we may wish to avoid. That said, the demo and the consensus can pick
-- different values here.
protocolInfo :: DemoProtocol p  -- ^ Protocol to run
             -> Int             -- ^ Our node ID (we must be a core node)
             -> Int             -- ^ Total number of core nodes in the system
             -> ProtocolInfo p
protocolInfo DemoBFT nid numCoreNodes = ProtocolInfo {
      initLedger     = ExtLedgerState (Mock.SimpleLedgerState mempty mempty) ()
    , nodeState      = ()
    , protocolConfig = BftNodeConfig {
          bftNodeId   = CoreId nid
        , bftSignKey  = SignKeyMockDSIGN nid
        , bftNumNodes = fromIntegral numCoreNodes
        , bftVerKeys  = Map.fromList [
              (CoreId n, VerKeyMockDSIGN n)
            | n <- [0 .. numCoreNodes - 1]
            ]
        }
    }
protocolInfo DemoPraos nid numCoreNodes = ProtocolInfo {
      initLedger = ExtLedgerState {
          ledgerState         = Mock.SimpleLedgerState mempty mempty
        , ouroborosChainState = []
        }
    , nodeState = SignKeyMockKES (
          fst $ verKeys IntMap.! nid
         , 0
         , 1 + fromIntegral numSlots
         )
    , protocolConfig = EncNodeConfig {
        encNodeConfigP = PraosNodeConfig {
            praosNodeId        = CoreId nid
          , praosSignKeyVRF    = SignKeyMockVRF nid
          , praosSlotsPerEpoch = fromIntegral $ k * kPerEpoch
          , praosInitialEta    = 0
          , praosInitialStake  = initialStake
          , praosLeaderF       = 0.5
          , praosK             = fromIntegral k
          , praosVerKeys       = verKeys
          }
      , encNodeConfigExt = Map.fromList [
            ("a", CoreId 0)
          , ("b", CoreId 1)
          , ("c", CoreId 2)
          ]
      }
    }
  where
    initialStake :: StakeDist
    initialStake =
        let q = recip $ fromIntegral numCoreNodes
        in  IntMap.fromList [(i, q) | i <- [0 .. numCoreNodes - 1]]

    verKeys :: IntMap (VerKeyKES MockKES, VerKeyVRF MockVRF)
    verKeys = IntMap.fromList [ (nd, (VerKeyMockKES nd, VerKeyMockVRF nd))
                              | nd <- [0 .. numCoreNodes - 1]
                              ]

    k, kPerEpoch, numSlots :: Int
    k         = 5
    kPerEpoch = 3
    numSlots  = maxBound

{-------------------------------------------------------------------------------
  Required instances
-------------------------------------------------------------------------------}

instance HasPayload (Praos PraosMockCrypto) (Block DemoPraos) where
  blockPayload _ = encPayloadP
                 . Mock.headerOuroboros
                 . Mock.simpleHeader

instance ProtocolLedgerView (Block DemoBFT) where
  protocolLedgerView _ _ = ()

instance ProtocolLedgerView (Block DemoPraos) where
  protocolLedgerView (EncNodeConfig _nodeConfig extCfg) (Mock.SimpleLedgerState u _) =
      relativeStakes $ totalStakes extCfg u

{-------------------------------------------------------------------------------
  Praos auxiliary
-------------------------------------------------------------------------------}

relativeStakes :: Map (Maybe Int) Int -> StakeDist
relativeStakes m =
   let totalStake    = fromIntegral $ sum $ Map.elems m
   in  IntMap.fromList [ (nid, fromIntegral stake / totalStake)
                       | (Just nid, stake) <- Map.toList m
                       ]

totalStakes :: Map Mock.Addr NodeId
            -> Mock.Utxo
            -> Map (Maybe Int) Int
totalStakes addrDist = foldl f Map.empty
 where
   f :: Map (Maybe Int) Int -> Mock.TxOut -> Map (Maybe Int) Int
   f m (a, stake) = case Map.lookup a addrDist of
       Just (CoreId nid) -> Map.insertWith (+) (Just nid) stake m
       _                 -> Map.insertWith (+) Nothing stake m
