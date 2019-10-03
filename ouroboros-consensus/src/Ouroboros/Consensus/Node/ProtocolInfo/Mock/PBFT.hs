{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Node.ProtocolInfo.Mock.PBFT (
    protocolInfoMockPBFT
  ) where

import           Codec.Serialise (Serialise (..))
import qualified Data.Bimap as Bimap

import           Cardano.Crypto.DSIGN

import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Mock
import           Ouroboros.Consensus.Node.ProtocolInfo.Abstract
import           Ouroboros.Consensus.NodeId (CoreNodeId (..))
import           Ouroboros.Consensus.Protocol.ExtNodeConfig
import           Ouroboros.Consensus.Protocol.PBFT
import qualified Ouroboros.Consensus.Protocol.PBFT.ChainState as CS

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
              , pbftIsLeader = PBftIsALeader PBftIsLeader {
                    pbftCoreNodeId = CoreNodeId nid
                  , pbftSignKey    = SignKeyMockDSIGN nid
                    -- For Mock PBFT, we use our key as the genesis key.
                  , pbftDlgCert    = (VerKeyMockDSIGN nid, VerKeyMockDSIGN nid)
                  }
              }
            , encNodeConfigExt = PBftLedgerView $
                Bimap.fromList [ (VerKeyMockDSIGN n, VerKeyMockDSIGN n)
                               | n <- [0 .. numCoreNodes - 1]
                               ]
          }
      , pInfoInitLedger = ExtLedgerState (genesisSimpleLedgerState addrDist)
                                         CS.empty
      , pInfoInitState  = ()
      }
  where
    addrDist :: AddrDist
    addrDist = mkAddrDist numCoreNodes

instance Serialise (VerKeyDSIGN MockDSIGN) where
  encode = encodeVerKeyDSIGN
  decode = decodeVerKeyDSIGN
