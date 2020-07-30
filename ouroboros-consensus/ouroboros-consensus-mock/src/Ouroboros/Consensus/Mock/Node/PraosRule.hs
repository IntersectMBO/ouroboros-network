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
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Mock.Ledger
import           Ouroboros.Consensus.Mock.Node
import           Ouroboros.Consensus.Mock.Protocol.Praos
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId (CoreNodeId (..))
import           Ouroboros.Consensus.Protocol.LeaderSchedule

type MockPraosRuleBlock = SimplePraosRuleBlock SimpleMockCrypto

protocolInfoPraosRule :: Monad m
                      => NumCoreNodes
                      -> CoreNodeId
                      -> PraosParams
                      -> HardFork.EraParams
                      -> LeaderSchedule
                      -> ProtocolInfo m MockPraosRuleBlock
protocolInfoPraosRule numCoreNodes
                      nid
                      params
                      eraParams
                      schedule =
    ProtocolInfo {
      pInfoConfig = TopLevelConfig {
          topLevelConfigProtocol = WLSConfig {
              wlsConfigSchedule = schedule
            , wlsConfigP        = PraosConfig
                { praosParams       = params
                , praosSignKeyVRF   = NeverUsedSignKeyVRF
                , praosInitialEta   = 0
                , praosInitialStake = genesisStakeDist addrDist
                , praosVerKeys      = verKeys
                }
            , wlsConfigNodeId   = nid
            }
        , topLevelConfigBlock = FullBlockConfig {
              blockConfigLedger = SimpleLedgerConfig () eraParams
            , blockConfigBlock  = SimpleBlockConfig (praosSecurityParam params)
            , blockConfigCodec  = SimpleCodecConfig (praosSecurityParam params)
            }
        }
    , pInfoInitLedger = ExtLedgerState
        { ledgerState = genesisSimpleLedgerState addrDist
        , headerState = genesisHeaderState ()
        }
    , pInfoBlockForging = Just (return (simpleBlockForging () forgePraosRuleExt))
    }
  where
    addrDist :: AddrDist
    addrDist = mkAddrDist numCoreNodes

    verKeys :: Map CoreNodeId (VerKeyKES NeverKES, VerKeyVRF NeverVRF)
    verKeys = Map.fromList [ (nid', (NeverUsedVerKeyKES, NeverUsedVerKeyVRF))
                           | nid' <- enumCoreNodes numCoreNodes
                           ]
