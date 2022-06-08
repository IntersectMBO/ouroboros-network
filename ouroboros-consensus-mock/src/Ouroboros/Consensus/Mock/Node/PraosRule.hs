-- | Test the Praos chain selection rule but with explicit leader schedule
module Ouroboros.Consensus.Mock.Node.PraosRule (
    MockPraosRuleBlock
  , blockForgingPraosRule
  , protocolInfoPraosRule
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Cardano.Crypto.KES
import           Cardano.Crypto.VRF

import           Ouroboros.Consensus.Block.Forging
import           Ouroboros.Consensus.Config
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Mock.Ledger
import           Ouroboros.Consensus.Mock.Node
import           Ouroboros.Consensus.Mock.Protocol.LeaderSchedule
import           Ouroboros.Consensus.Mock.Protocol.Praos
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId (CoreNodeId (..))

type MockPraosRuleBlock = SimplePraosRuleBlock SimpleMockCrypto

protocolInfoPraosRule :: NumCoreNodes
                      -> CoreNodeId
                      -> PraosParams
                      -> HardFork.EraParams
                      -> LeaderSchedule
                      -> PraosEvolvingStake
                      -> ProtocolInfo MockPraosRuleBlock
protocolInfoPraosRule numCoreNodes
                      nid
                      params
                      eraParams
                      schedule
                      evolvingStake =
    ProtocolInfo {
      pInfoConfig = TopLevelConfig {
          topLevelConfigProtocol = WLSConfig {
              wlsConfigSchedule = schedule
            , wlsConfigP        = PraosConfig
                { praosParams        = params
                , praosSignKeyVRF    = NeverUsedSignKeyVRF
                , praosInitialEta    = 0
                , praosInitialStake  = genesisStakeDist addrDist
                , praosEvolvingStake = evolvingStake
                , praosVerKeys       = verKeys
                }
            , wlsConfigNodeId   = nid
            }
        , topLevelConfigLedger  = SimpleLedgerConfig () eraParams
        , topLevelConfigBlock   = SimpleBlockConfig
        , topLevelConfigCodec   = SimpleCodecConfig
        , topLevelConfigStorage = SimpleStorageConfig (praosSecurityParam params)
        }
    , pInfoInitLedger = ExtLedgerState
        { ledgerState = genesisSimpleLedgerState addrDist
        , headerState = genesisHeaderState ()
        }
    }
  where
    addrDist :: AddrDist
    addrDist = mkAddrDist numCoreNodes

    verKeys :: Map CoreNodeId (VerKeyKES NeverKES, VerKeyVRF NeverVRF)
    verKeys = Map.fromList [ (nid', (NeverUsedVerKeyKES, NeverUsedVerKeyVRF))
                           | nid' <- enumCoreNodes numCoreNodes
                           ]

blockForgingPraosRule :: Monad m => [BlockForging m MockPraosRuleBlock]
blockForgingPraosRule = [simpleBlockForging () forgePraosRuleExt]
