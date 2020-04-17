{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Mock.Node.PBFT (
    MockPBftBlock
  , protocolInfoMockPBFT
  ) where

import qualified Data.Bimap as Bimap

import           Cardano.Crypto.DSIGN

import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Mock.Ledger
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId (CoreNodeId (..))
import           Ouroboros.Consensus.Protocol.PBFT
import qualified Ouroboros.Consensus.Protocol.PBFT.State as S

type MockPBftBlock = SimplePBftBlock SimpleMockCrypto PBftMockCrypto

protocolInfoMockPBFT :: PBftParams
                     -> BlockConfig MockPBftBlock
                     -> CoreNodeId
                     -> ProtocolInfo MockPBftBlock
protocolInfoMockPBFT params cfg nid =
    ProtocolInfo {
        pInfoConfig = TopLevelConfig {
            configConsensus = PBftConfig {
                 pbftParams   = params
               , pbftIsLeader = PBftIsALeader PBftIsLeader {
                     pbftCoreNodeId = nid
                   , pbftSignKey    = signKey nid
                     -- For Mock PBFT, we use our key as the genesis key.
                   , pbftDlgCert    = (verKey nid, verKey nid)
                   }
               }
          , configLedger = ledgerView
          , configBlock  = cfg
          }
      , pInfoInitLedger = ExtLedgerState (genesisSimpleLedgerState addrDist)
                                         (genesisHeaderState S.empty)
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
