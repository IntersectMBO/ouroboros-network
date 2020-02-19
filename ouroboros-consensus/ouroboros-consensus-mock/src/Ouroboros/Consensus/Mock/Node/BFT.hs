module Ouroboros.Consensus.Mock.Node.BFT (
    protocolInfoBft
  ) where

import qualified Data.Map.Strict as Map

import           Cardano.Crypto.DSIGN

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Mock.Ledger
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId (CoreNodeId (..), NodeId (..))
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.BFT

protocolInfoBft :: NumCoreNodes
                -> CoreNodeId
                -> SecurityParam
                -> SlotLengths
                -> ProtocolInfo (SimpleBftBlock SimpleMockCrypto BftMockCrypto)
protocolInfoBft numCoreNodes nid securityParam slotLengths =
    ProtocolInfo {
        pInfoConfig = TopLevelConfig {
            configConsensus = BftNodeConfig {
                bftParams   = BftParams {
                                  bftNumNodes      = numCoreNodes
                                , bftSecurityParam = securityParam
                                , bftSlotLengths   = slotLengths
                                }
              , bftNodeId   = CoreId nid
              , bftSignKey  = signKey nid
              , bftVerKeys  = Map.fromList [
                    (CoreId n, verKey n)
                  | n <- enumCoreNodes numCoreNodes
                  ]
              }
          , configLedger = SimpleLedgerConfig
          , configBlock  = SimpleBftBlockConfig
          }
      , pInfoInitLedger = ExtLedgerState (genesisSimpleLedgerState addrDist)
                                         (genesisHeaderState ())
      , pInfoInitState  = ()
      }
  where
    signKey :: CoreNodeId -> SignKeyDSIGN MockDSIGN
    signKey (CoreNodeId n) = SignKeyMockDSIGN (fromIntegral n)

    verKey :: CoreNodeId -> VerKeyDSIGN MockDSIGN
    verKey (CoreNodeId n) = VerKeyMockDSIGN (fromIntegral n)

    addrDist :: AddrDist
    addrDist = mkAddrDist numCoreNodes
