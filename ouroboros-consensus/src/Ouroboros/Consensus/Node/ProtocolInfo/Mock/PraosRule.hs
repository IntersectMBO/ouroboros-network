-- | Test the Praos chain selection rule but with explicit leader schedule
module Ouroboros.Consensus.Node.ProtocolInfo.Mock.PraosRule (
    protocolInfoPraosRule
  ) where

import           Data.Map (Map)
import qualified Data.Map as Map

import           Cardano.Crypto.KES
import           Cardano.Crypto.VRF

import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Mock
import           Ouroboros.Consensus.Node.ProtocolInfo.Abstract
import           Ouroboros.Consensus.NodeId (CoreNodeId (..), NodeId (..))
import           Ouroboros.Consensus.Protocol.LeaderSchedule
import           Ouroboros.Consensus.Protocol.Praos

protocolInfoPraosRule :: NumCoreNodes
                      -> CoreNodeId
                      -> PraosParams
                      -> LeaderSchedule
                      -> ProtocolInfo (SimplePraosRuleBlock SimpleMockCrypto)
protocolInfoPraosRule numCoreNodes
                      nid
                      params
                      schedule =
    ProtocolInfo {
      pInfoConfig    = WLSNodeConfig
        { lsNodeConfigSchedule = schedule
        , lsNodeConfigP        = PraosNodeConfig
            { praosParams       = params
            , praosNodeId       = CoreId nid
            , praosSignKeyVRF   = NeverUsedSignKeyVRF
            , praosInitialEta   = 0
            , praosInitialStake = genesisStakeDist addrDist
            , praosVerKeys      = verKeys
            }
        , lsNodeConfigNodeId   = nid
        }
    , pInfoInitLedger = ExtLedgerState
        { ledgerState         = genesisSimpleLedgerState addrDist
        , ouroborosChainState = ()
        }
    , pInfoInitState  = ()
    }
  where
    addrDist = mkAddrDist numCoreNodes

    verKeys :: Map CoreNodeId (VerKeyKES NeverKES, VerKeyVRF NeverVRF)
    verKeys = Map.fromList [ (nid', (NeverUsedVerKeyKES, NeverUsedVerKeyVRF))
                           | nid' <- enumCoreNodes numCoreNodes
                           ]
