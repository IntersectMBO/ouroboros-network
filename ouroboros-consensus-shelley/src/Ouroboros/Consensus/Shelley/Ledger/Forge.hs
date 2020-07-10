{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Shelley.Ledger.Forge (
    forgeShelleyBlock
  ) where

import           Control.Exception
import qualified Data.Sequence.Strict as Seq

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract

import qualified Shelley.Spec.Ledger.BlockChain as SL
import qualified Shelley.Spec.Ledger.Keys as SL

import           Ouroboros.Consensus.Shelley.Ledger.Block
import           Ouroboros.Consensus.Shelley.Ledger.Config
import           Ouroboros.Consensus.Shelley.Ledger.Integrity
import           Ouroboros.Consensus.Shelley.Ledger.Mempool
import           Ouroboros.Consensus.Shelley.Protocol

{-------------------------------------------------------------------------------
  CanForge
-------------------------------------------------------------------------------}

instance TPraosCrypto c => CanForge (ShelleyBlock c) where
  forgeBlock = forgeShelleyBlock

{-------------------------------------------------------------------------------
  Forging
-------------------------------------------------------------------------------}

forgeShelleyBlock
  :: TPraosCrypto c
  => TopLevelConfig (ShelleyBlock c)
  -> ForgeState (ShelleyBlock c)
  -> BlockNo                             -- ^ Current block number
  -> SlotNo                              -- ^ Current slot number
  -> TickedLedgerState (ShelleyBlock c)  -- ^ Current ledger
  -> [GenTx (ShelleyBlock c)]            -- ^ Txs to add in the block
  -> TPraosProof c                       -- ^ Leader proof ('IsLeader')
  -> ShelleyBlock c
forgeShelleyBlock cfg forgeState curNo curSlot tickedLedger txs isLeader =
    assert (verifyBlockIntegrity tpraosSlotsPerKESPeriod blk) blk
  where
    TPraosConfig { tpraosParams = TPraosParams { tpraosSlotsPerKESPeriod } } =
      configConsensus cfg

    hotKey       = chainIndepState forgeState
    tpraosFields = forgeTPraosFields hotKey isLeader mkBhBody
    blk          = mkShelleyBlock $ SL.Block (mkHeader tpraosFields) body
    body         = SL.TxSeq $ Seq.fromList $ (\(ShelleyTx _ tx) -> tx) <$> txs

    mkHeader TPraosFields { tpraosSignature, tpraosToSign } =
      SL.BHeader tpraosToSign tpraosSignature

    prevHash =
        toShelleyPrevHash
      . castHash
      . getTipHash
      $ tickedLedger

    mkBhBody toSign = SL.BHBody {
          bheaderPrev    = prevHash
        , bheaderVk      = SL.VKey tpraosToSignIssuerVK
        , bheaderVrfVk   = tpraosToSignVrfVK
        , bheaderSlotNo  = curSlot
        , bheaderBlockNo = curNo
        , bheaderEta     = tpraosToSignEta
        , bheaderL       = tpraosToSignLeader
        , bsize          = fromIntegral $ SL.bBodySize body
        , bhash          = SL.bbHash body
        , bheaderOCert   = tpraosToSignOCert
        , bprotver       = shelleyProtocolVersion $ configBlock cfg
        }
      where
        TPraosToSign {
            tpraosToSignIssuerVK
          , tpraosToSignVrfVK
          , tpraosToSignEta
          , tpraosToSignLeader
          , tpraosToSignOCert
          } = toSign
