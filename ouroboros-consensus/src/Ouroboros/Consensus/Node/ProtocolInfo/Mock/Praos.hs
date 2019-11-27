{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Node.ProtocolInfo.Mock.Praos (
    protocolInfoPraos
  ) where

import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

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
protocolInfoPraos (NumCoreNodes numCoreNodes) (CoreNodeId nid) params =
    ProtocolInfo {
        pInfoConfig = PraosNodeConfig {
            praosParams       = params
          , praosNodeId       = CoreId nid
          , praosSignKeyVRF   = SignKeyMockVRF nid
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
           (fst $ verKeys IntMap.! nid)   -- key ID
           0                              -- KES initial slot
           (praosLifetimeKES params)      -- KES lifetime
      }
  where
    addrDist :: AddrDist
    addrDist = mkAddrDist numCoreNodes

    verKeys :: IntMap (VerKeyKES MockKES, VerKeyVRF MockVRF)
    verKeys = IntMap.fromList [ (nd, (VerKeyMockKES nd, VerKeyMockVRF nd))
                              | nd <- [0 .. numCoreNodes - 1]
                              ]
