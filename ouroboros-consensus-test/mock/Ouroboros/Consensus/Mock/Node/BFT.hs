module Ouroboros.Consensus.Mock.Node.BFT (
    MockBftBlock
  , protocolInfoBft
  ) where

import           Cardano.Crypto.DSIGN
import qualified Data.Map.Strict as Map
import           Ouroboros.Consensus.Config
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Mock.Ledger
import           Ouroboros.Consensus.Mock.Node
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId (CoreNodeId (..), NodeId (..))
import           Ouroboros.Consensus.Protocol.BFT

type MockBftBlock = SimpleBftBlock SimpleMockCrypto BftMockCrypto

protocolInfoBft :: Monad m
                => NumCoreNodes
                -> CoreNodeId
                -> SecurityParam
                -> HardFork.EraParams
                -> ProtocolInfo m MockBftBlock
protocolInfoBft numCoreNodes nid securityParam eraParams =
    ProtocolInfo {
        pInfoConfig = TopLevelConfig {
            topLevelConfigProtocol = BftConfig {
                bftParams   = BftParams {
                                  bftNumNodes      = numCoreNodes
                                , bftSecurityParam = securityParam
                                }
              , bftSignKey  = signKey nid
              , bftVerKeys  = Map.fromList [
                    (CoreId n, verKey n)
                  | n <- enumCoreNodes numCoreNodes
                  ]
              }
          , topLevelConfigLedger  = SimpleLedgerConfig () eraParams
          , topLevelConfigBlock   = SimpleBlockConfig
          , topLevelConfigCodec   = SimpleCodecConfig
          , topLevelConfigStorage = SimpleStorageConfig securityParam
          }
      , pInfoInitLedger = ExtLedgerState (genesisSimpleLedgerState addrDist)
                                         (genesisHeaderState ())
      , pInfoBlockForging = return [simpleBlockForging nid forgeBftExt]
      }
  where
    signKey :: CoreNodeId -> SignKeyDSIGN MockDSIGN
    signKey (CoreNodeId n) = SignKeyMockDSIGN n

    verKey :: CoreNodeId -> VerKeyDSIGN MockDSIGN
    verKey (CoreNodeId n) = VerKeyMockDSIGN n

    addrDist :: AddrDist
    addrDist = mkAddrDist numCoreNodes
