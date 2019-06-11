module Ouroboros.Consensus.Demo.Ledger.Mock.BFT (
    protocolInfoBft
  ) where

import qualified Data.Map as Map

import           Ouroboros.Consensus.Crypto.DSIGN
import           Ouroboros.Consensus.Demo.Run
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Mock
import           Ouroboros.Consensus.NodeId (CoreNodeId (..), NodeId (..))
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.BFT

protocolInfoBft :: NumCoreNodes
                -> CoreNodeId
                -> SecurityParam
                -> ProtocolInfo (SimpleBftBlock SimpleMockCrypto BftMockCrypto)
protocolInfoBft (NumCoreNodes numCoreNodes) (CoreNodeId nid) securityParam =
    ProtocolInfo {
        pInfoConfig = BftNodeConfig {
            bftParams   = BftParams {
                              bftNumNodes      = fromIntegral numCoreNodes
                            , bftSecurityParam = securityParam
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
