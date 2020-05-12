module Ouroboros.Consensus.Mock.Node.BFT (
    MockBftBlock
  , protocolInfoBft
  ) where

import qualified Data.Map.Strict as Map

import           Cardano.Crypto.DSIGN

import           Ouroboros.Consensus.Config
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Mock.Ledger
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId (CoreNodeId (..), NodeId (..))
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.BFT

type MockBftBlock = SimpleBftBlock SimpleMockCrypto BftMockCrypto

protocolInfoBft :: NumCoreNodes
                -> CoreNodeId
                -> SecurityParam
                -> HardFork.EraParams
                -> ProtocolInfo MockBftBlock
protocolInfoBft numCoreNodes nid securityParam eraParams =
    ProtocolInfo {
        pInfoConfig = TopLevelConfig {
            configConsensus = BftConfig {
                bftParams   = BftParams {
                                  bftNumNodes      = numCoreNodes
                                , bftSecurityParam = securityParam
                                }
              , bftNodeId   = CoreId nid
              , bftSignKey  = signKey nid
              , bftVerKeys  = Map.fromList [
                    (CoreId n, verKey n)
                  | n <- enumCoreNodes numCoreNodes
                  ]
              }
          , configLedger = SimpleLedgerConfig () eraParams
          , configBlock  = SimpleBlockConfig
          }
      , pInfoInitLedger = ExtLedgerState (genesisSimpleLedgerState addrDist)
                                         (genesisHeaderState ())
      , pInfoInitState  = ()
      }
  where
    signKey :: CoreNodeId -> SignKeyDSIGN MockDSIGN
    signKey (CoreNodeId n) = SignKeyMockDSIGN n

    verKey :: CoreNodeId -> VerKeyDSIGN MockDSIGN
    verKey (CoreNodeId n) = VerKeyMockDSIGN n

    addrDist :: AddrDist
    addrDist = mkAddrDist numCoreNodes
