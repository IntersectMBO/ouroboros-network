{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Mock.Node.Praos (
    MockPraosBlock
  , protocolInfoPraos
  ) where

import           Data.Map (Map)
import qualified Data.Map as Map

import           Cardano.Crypto.KES
import           Cardano.Crypto.VRF

import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Mock.Ledger
import           Ouroboros.Consensus.Mock.Protocol.Praos
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId (CoreNodeId (..), NodeId (..))

type MockPraosBlock = SimplePraosBlock SimpleMockCrypto PraosMockCrypto

protocolInfoPraos :: NumCoreNodes
                  -> CoreNodeId
                  -> PraosParams
                  -> BlockConfig MockPraosBlock
                  -> ProtocolInfo MockPraosBlock
protocolInfoPraos numCoreNodes nid params cfg =
    ProtocolInfo {
        pInfoConfig = TopLevelConfig {
            configConsensus = PraosConfig {
                praosParams       = params
              , praosNodeId       = CoreId nid
              , praosSignKeyVRF   = signKeyVRF nid
              , praosInitialEta   = 0
              , praosInitialStake = genesisStakeDist addrDist
              , praosVerKeys      = verKeys
              }
          , configLedger = addrDist
          , configBlock  = cfg
          }
      , pInfoInitLedger = ExtLedgerState {
            ledgerState = genesisSimpleLedgerState addrDist
          , headerState = genesisHeaderState []
          }
      , pInfoInitState = PraosKeyAvailable $ SignKeyMockKES
           (fst $ verKeys Map.! nid)   -- key ID
           0                           -- KES initial slot
           (praosLifetimeKES params)   -- KES lifetime
      }
  where
    signKeyVRF :: CoreNodeId -> SignKeyVRF MockVRF
    signKeyVRF (CoreNodeId n) = SignKeyMockVRF (fromIntegral n)

    verKeyVRF :: CoreNodeId -> VerKeyVRF MockVRF
    verKeyVRF (CoreNodeId n) = VerKeyMockVRF (fromIntegral n)

    verKeyKES :: CoreNodeId -> VerKeyKES MockKES
    verKeyKES (CoreNodeId n) = VerKeyMockKES (fromIntegral n)

    addrDist :: AddrDist
    addrDist = mkAddrDist numCoreNodes

    verKeys :: Map CoreNodeId (VerKeyKES MockKES, VerKeyVRF MockVRF)
    verKeys = Map.fromList
      [ (nid', (kesKey, vrfKey))
      | nid' <- enumCoreNodes numCoreNodes
      , let !kesKey = verKeyKES nid'
            !vrfKey = verKeyVRF nid'
      ]
