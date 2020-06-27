module Ouroboros.Consensus.Mock.Node.BFT (
    MockBftBlock
  , protocolInfoBft
  ) where

import qualified Data.Map.Strict as Map

import           Cardano.Crypto.DSIGN

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Config.SecurityParam
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Mock.Ledger
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
            topLevelConfigProtocol = FullProtocolConfig {
                protocolConfigConsensus = BftConfig {
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
              , protocolConfigIndep = ()
              }
          , topLevelConfigBlock = FullBlockConfig {
                blockConfigLedger = SimpleLedgerConfig () eraParams
              , blockConfigBlock  = SimpleBlockConfig securityParam
              , blockConfigCodec  = SimpleCodecConfig securityParam
              }
          }
      , pInfoInitLedger = ExtLedgerState (genesisSimpleLedgerState addrDist)
                                         (genesisHeaderState ())
      , pInfoLeaderCreds = Just (
            nid
          , defaultMaintainForgeState
          )
      }
  where
    signKey :: CoreNodeId -> SignKeyDSIGN MockDSIGN
    signKey (CoreNodeId n) = SignKeyMockDSIGN n

    verKey :: CoreNodeId -> VerKeyDSIGN MockDSIGN
    verKey (CoreNodeId n) = VerKeyMockDSIGN n

    addrDist :: AddrDist
    addrDist = mkAddrDist numCoreNodes
