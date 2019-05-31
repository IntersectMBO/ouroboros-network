module Ouroboros.Consensus.Demo.Ledger.Mock.PBFT (
    protocolInfoMockPBFT
  ) where

import qualified Data.Bimap as Bimap
import qualified Data.Sequence as Seq

import           Ouroboros.Consensus.Crypto.DSIGN
import           Ouroboros.Consensus.Demo.Run
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Mock
import           Ouroboros.Consensus.Node (CoreNodeId (..), NodeId (..))
import           Ouroboros.Consensus.Protocol.ExtNodeConfig
import           Ouroboros.Consensus.Protocol.PBFT

protocolInfoMockPBFT :: NumCoreNodes
                     -> CoreNodeId
                     -> PBftParams
                     -> ProtocolInfo (SimplePBftBlock SimpleMockCrypto
                                                      PBftMockCrypto)
protocolInfoMockPBFT (NumCoreNodes numCoreNodes) (CoreNodeId nid) params =
    ProtocolInfo {
        pInfoConfig = EncNodeConfig {
            encNodeConfigP = PBftNodeConfig {
                pbftParams   = params {pbftNumNodes = fromIntegral numCoreNodes}
              , pbftNodeId   = CoreId nid
              , pbftSignKey  = SignKeyMockDSIGN nid
              , pbftVerKey   = VerKeyMockDSIGN nid
              }
            , encNodeConfigExt = PBftLedgerView $
                Bimap.fromList [ (VerKeyMockDSIGN n, VerKeyMockDSIGN n)
                               | n <- [0 .. numCoreNodes - 1]
                               ]
          }
      , pInfoInitLedger = ExtLedgerState (genesisSimpleLedgerState addrDist)
                                         Seq.empty
      , pInfoInitState  = ()
      }
  where
    addrDist :: AddrDist
    addrDist = mkAddrDist numCoreNodes
