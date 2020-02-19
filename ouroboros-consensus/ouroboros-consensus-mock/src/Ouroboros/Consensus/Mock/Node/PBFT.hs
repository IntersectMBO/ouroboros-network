{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Mock.Node.PBFT (
    protocolInfoMockPBFT
  ) where

import qualified Data.Bimap as Bimap

import           Cardano.Crypto.DSIGN

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Mock.Ledger
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId (CoreNodeId (..))
import           Ouroboros.Consensus.Protocol.PBFT
import qualified Ouroboros.Consensus.Protocol.PBFT.ChainState as CS

protocolInfoMockPBFT :: PBftParams
                     -> SlotLengths
                     -> CoreNodeId
                     -> ProtocolInfo (SimplePBftBlock SimpleMockCrypto
                                                      PBftMockCrypto)
protocolInfoMockPBFT params slotLengths nid =
    ProtocolInfo {
        pInfoConfig = TopLevelConfig {
            configConsensus = PBftNodeConfig {
                 pbftParams   = params
               , pbftIsLeader = PBftIsALeader PBftIsLeader {
                     pbftCoreNodeId = nid
                   , pbftSignKey    = signKey nid
                     -- For Mock PBFT, we use our key as the genesis key.
                   , pbftDlgCert    = (verKey nid, verKey nid)
                   }
               }
          , configLedger = SimpleLedgerConfig
          , configBlock  = SimplePBftBlockConfig ledgerView slotLengths
          }
      , pInfoInitLedger = ExtLedgerState (genesisSimpleLedgerState addrDist)
                                         (genesisHeaderState CS.empty)
      , pInfoInitState  = ()
      }
  where
    ledgerView :: PBftLedgerView PBftMockCrypto
    ledgerView = PBftLedgerView $
        Bimap.fromList [ (verKey n, verKey n)
                       | n <- enumCoreNodes (pbftNumNodes params)
                       ]

    signKey :: CoreNodeId -> SignKeyDSIGN MockDSIGN
    signKey (CoreNodeId n) = SignKeyMockDSIGN (fromIntegral n)

    verKey :: CoreNodeId -> VerKeyDSIGN MockDSIGN
    verKey (CoreNodeId n) = VerKeyMockDSIGN (fromIntegral n)

    addrDist :: AddrDist
    addrDist = mkAddrDist (pbftNumNodes params)
