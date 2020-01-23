{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Node.ProtocolInfo.Mock.Praos (
    protocolInfoPraos
  ) where

import           Data.Map (Map)
import qualified Data.Map as Map

import           Cardano.Crypto.KES
import           Cardano.Crypto.VRF

import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Mock
import           Ouroboros.Consensus.Node.ProtocolInfo.Abstract
import           Ouroboros.Consensus.NodeId (CoreNodeId (..), NodeId (..))
import           Ouroboros.Consensus.Protocol.Praos

protocolInfoPraos :: NumCoreNodes
                  -> CoreNodeId
                  -> PraosParams
                  -> ProtocolInfo (SimplePraosBlock SimpleMockCrypto
                                                    PraosMockCrypto)
protocolInfoPraos numCoreNodes nid params =
    ProtocolInfo {
        pInfoConfig = PraosNodeConfig {
            praosParams       = params
          , praosNodeId       = CoreId nid
          , praosSignKeyVRF   = signKeyVRF nid
          , praosInitialEta   = 0
          , praosInitialStake = genesisStakeDist addrDist
          , praosVerKeys      = verKeys
          , praosExtConfig    = addrDist
          }
      , pInfoInitLedger = ExtLedgerState {
            ledgerState         = genesisSimpleLedgerState addrDist
          , ouroborosChainState = []
          }
      , pInfoInitState = PraosNodeState $ SignKeyMockKES
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
    verKeys = Map.fromList [ (nid', (verKeyKES nid', verKeyVRF nid'))
                           | nid' <- enumCoreNodes numCoreNodes
                           ]
