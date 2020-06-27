-- | Test the Praos chain selection rule but with explicit leader schedule
module Ouroboros.Consensus.Mock.Node.PraosRule (
    MockPraosRuleBlock
  , protocolInfoPraosRule
  ) where

import           Data.Map (Map)
import qualified Data.Map as Map

import           Cardano.Crypto.KES
import           Cardano.Crypto.VRF

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Mock.Ledger
import           Ouroboros.Consensus.Mock.Protocol.Praos
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId (CoreNodeId (..))
import           Ouroboros.Consensus.Protocol.LeaderSchedule
import           Ouroboros.Consensus.Util.IOLike

type MockPraosRuleBlock = SimplePraosRuleBlock SimpleMockCrypto

protocolInfoPraosRule :: IOLike m
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
          configConsensus = WLSConfig {
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
        , configIndep  = ()
        , configLedger = SimpleLedgerConfig () eraParams
        , configBlock  = SimpleBlockConfig (praosSecurityParam params)
        , configCodec  = SimpleCodecConfig (praosSecurityParam params)
        }
    , pInfoInitLedger = ExtLedgerState
        { ledgerState = genesisSimpleLedgerState addrDist
        , headerState = genesisHeaderState ()
        }
    , pInfoLeaderCreds = Just (
          ()
        , defaultMaintainForgeState
        )
    }
  where
    addrDist = mkAddrDist numCoreNodes

    verKeys :: Map CoreNodeId (VerKeyKES NeverKES, VerKeyVRF NeverVRF)
    verKeys = Map.fromList [ (nid', (NeverUsedVerKeyKES, NeverUsedVerKeyVRF))
                           | nid' <- enumCoreNodes numCoreNodes
                           ]
