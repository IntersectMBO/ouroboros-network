{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Mock.Node.PBFT (
    MockPBftBlock
  , protocolInfoMockPBFT
  ) where

import qualified Data.Bimap as Bimap

import           Cardano.Crypto.DSIGN

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsMempool (txForgetValidated)
import           Ouroboros.Consensus.Mock.Ledger
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId (CoreNodeId (..))
import           Ouroboros.Consensus.Protocol.PBFT
import qualified Ouroboros.Consensus.Protocol.PBFT.State as S

type MockPBftBlock = SimplePBftBlock SimpleMockCrypto PBftMockCrypto

protocolInfoMockPBFT :: Monad m
                     => PBftParams
                     -> HardFork.EraParams
                     -> CoreNodeId
                     -> ProtocolInfo m MockPBftBlock
protocolInfoMockPBFT params eraParams nid =
    ProtocolInfo {
        pInfoConfig = TopLevelConfig {
            topLevelConfigProtocol = PBftConfig {
                pbftParams = params
              }
          , topLevelConfigLedger  = SimpleLedgerConfig ledgerView eraParams
          , topLevelConfigBlock   = SimpleBlockConfig
          , topLevelConfigCodec   = SimpleCodecConfig
          , topLevelConfigStorage = SimpleStorageConfig (pbftSecurityParam params)
          }
      , pInfoInitLedger = ExtLedgerState (genesisSimpleLedgerState addrDist)
                                         (genesisHeaderState S.empty)
      , pInfoBlockForging = return [pbftBlockForging canBeLeader]
      }
  where
    canBeLeader :: PBftCanBeLeader PBftMockCrypto
    canBeLeader = PBftCanBeLeader {
          pbftCanBeLeaderCoreNodeId = nid
        , pbftCanBeLeaderSignKey    = signKey nid
          -- For Mock PBFT, we use our key as the genesis key.
        , pbftCanBeLeaderDlgCert    = (verKey nid, verKey nid)
        }

    ledgerView :: PBftLedgerView PBftMockCrypto
    ledgerView = PBftLedgerView $ Bimap.fromList [
          (PBftMockVerKeyHash (verKey n), PBftMockVerKeyHash (verKey n))
        | n <- enumCoreNodes (pbftNumNodes params)
        ]

    signKey :: CoreNodeId -> SignKeyDSIGN MockDSIGN
    signKey (CoreNodeId n) = SignKeyMockDSIGN n

    verKey :: CoreNodeId -> VerKeyDSIGN MockDSIGN
    verKey (CoreNodeId n) = VerKeyMockDSIGN n

    addrDist :: AddrDist
    addrDist = mkAddrDist (pbftNumNodes params)

{-------------------------------------------------------------------------------
  BlockForging
-------------------------------------------------------------------------------}

pbftBlockForging ::
     ( SimpleCrypto c
     , PBftCrypto c'
     , Signable (PBftDSIGN c') (SignedSimplePBft c c')
     , ContextDSIGN (PBftDSIGN c') ~ ()
     , Monad m
     )
  => PBftCanBeLeader c'
  -> BlockForging m (SimplePBftBlock c c')
pbftBlockForging canBeLeader = BlockForging {
      forgeLabel       = "pbftBlockForging"
    , canBeLeader
    , updateForgeState = \_ _ _ -> return $ ForgeStateUpdated ()
    , checkCanForge    = \cfg slot tickedPBftState _isLeader ->
                           return $
                             pbftCheckCanForge
                               (configConsensus cfg)
                               canBeLeader
                               slot
                               tickedPBftState
    , forgeBlock       = \cfg slot bno lst txs proof ->
        return
          $ forgeSimple
              forgePBftExt
              cfg
              slot
              bno
              lst
              (map txForgetValidated txs)
              proof
    }
