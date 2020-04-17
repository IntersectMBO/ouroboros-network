-- | Test the Praos chain selection rule but with explicit leader schedule
module Ouroboros.Consensus.Mock.Node.PraosRule (
    MockPraosRuleBlock
  , protocolInfoPraosRule
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
import           Ouroboros.Consensus.Protocol.LeaderSchedule

type MockPraosRuleBlock = SimplePraosRuleBlock SimpleMockCrypto

protocolInfoPraosRule :: NumCoreNodes
                      -> CoreNodeId
                      -> PraosParams
                      -> BlockConfig MockPraosRuleBlock
                      -> LeaderSchedule
                      -> ProtocolInfo MockPraosRuleBlock
protocolInfoPraosRule numCoreNodes
                      nid
                      params
                      cfg
                      schedule =
    ProtocolInfo {
      pInfoConfig = TopLevelConfig {
          configConsensus = WLSConfig {
              wlsConfigSchedule = schedule
            , wlsConfigP        = PraosConfig
                { praosParams       = params
                , praosNodeId       = CoreId nid
                , praosSignKeyVRF   = NeverUsedSignKeyVRF
                , praosInitialEta   = 0
                , praosInitialStake = genesisStakeDist addrDist
                , praosVerKeys      = verKeys
                }
            , wlsConfigNodeId   = nid
            }
        , configLedger = ()
        , configBlock  = cfg
        }
    , pInfoInitLedger = ExtLedgerState
        { ledgerState = genesisSimpleLedgerState addrDist
        , headerState = genesisHeaderState ()
        }
    , pInfoInitState  = ()
    }
  where
    addrDist = mkAddrDist numCoreNodes

    verKeys :: Map CoreNodeId (VerKeyKES NeverKES, VerKeyVRF NeverVRF)
    verKeys = Map.fromList [ (nid', (NeverUsedVerKeyKES, NeverUsedVerKeyVRF))
                           | nid' <- enumCoreNodes numCoreNodes
                           ]
