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
import           Ouroboros.Consensus.Protocol.PBFT
import qualified Ouroboros.Consensus.Protocol.PBFT.ChainState as CS

protocolInfoMockPBFT :: PBftParams
                     -> CoreNodeId
                     -> ProtocolInfo (SimplePBftBlock SimpleMockCrypto
                                                      PBftMockCrypto)
protocolInfoMockPBFT params nid =
    ProtocolInfo {
        pInfoConfig = PBftNodeConfig {
            pbftParams   = params
          , pbftIsLeader = PBftIsALeader PBftIsLeader {
                pbftCoreNodeId = nid
              , pbftSignKey    = signKey nid
                -- For Mock PBFT, we use our key as the genesis key.
              , pbftDlgCert    = (verKey nid, verKey nid)
              }
          , pbftExtConfig = PBftLedgerView $
              Bimap.fromList [ (verKey n, verKey n)
                             | n <- enumCoreNodes (pbftNumNodes params)
                             ]
          }
      , pInfoInitLedger = ExtLedgerState (genesisSimpleLedgerState addrDist)
                                         CS.empty
      , pInfoInitState  = ()
      }
  where
    signKey :: CoreNodeId -> SignKeyDSIGN MockDSIGN
    signKey (CoreNodeId n) = SignKeyMockDSIGN (fromIntegral n)

    verKey :: CoreNodeId -> VerKeyDSIGN MockDSIGN
    verKey (CoreNodeId n) = VerKeyMockDSIGN (fromIntegral n)

    addrDist :: AddrDist
    addrDist = mkAddrDist (pbftNumNodes params)

instance Serialise (VerKeyDSIGN MockDSIGN) where
  encode = encodeVerKeyDSIGN
  decode = decodeVerKeyDSIGN
