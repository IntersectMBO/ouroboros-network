module Ouroboros.Consensus.Node.ProtocolInfo.Mock.BFT (
    protocolInfoBft
  ) where

import qualified Data.Map as Map

import           Cardano.Crypto.DSIGN

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Mock
import           Ouroboros.Consensus.Node.ProtocolInfo.Abstract
import           Ouroboros.Consensus.NodeId (CoreNodeId (..), NodeId (..))
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.BFT

protocolInfoBft :: NumCoreNodes
                -> CoreNodeId
                -> SecurityParam
                -> SlotLengths
                -> ProtocolInfo (SimpleBftBlock SimpleMockCrypto BftMockCrypto)
protocolInfoBft (NumCoreNodes numCoreNodes) (CoreNodeId nid) securityParam slotLengths =
    ProtocolInfo {
        pInfoConfig = BftNodeConfig {
            bftParams   = BftParams {
                              bftNumNodes      = fromIntegral numCoreNodes
                            , bftSecurityParam = securityParam
                            , bftSlotLengths   = slotLengths
                            }
          , bftNodeId   = CoreId nid
          , bftSignKey  = SignKeyMockDSIGN nid
          , bftVerKeys  = Map.fromList [
                (CoreId n, VerKeyMockDSIGN n)
              | n <- [0 .. numCoreNodes - 1]
              ]
          }
      , pInfoInitLedger = ExtLedgerState (genesisSimpleLedgerState addrDist) ()
      , pInfoInitState  = ()
      }
  where
    addrDist :: AddrDist
    addrDist = mkAddrDist numCoreNodes
