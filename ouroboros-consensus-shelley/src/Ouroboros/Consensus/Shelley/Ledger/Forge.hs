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

module Ouroboros.Consensus.Shelley.Ledger.Forge (
    forgeShelleyBlock
  ) where

import           Control.Exception
import           Control.Monad.Except
import           Data.List (foldl')
import qualified Data.Sequence.Strict as Seq

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Util.Assert

import qualified Shelley.Spec.Ledger.BlockChain as SL
import qualified Shelley.Spec.Ledger.Keys as SL

import           Ouroboros.Consensus.Shelley.Ledger.Block
import           Ouroboros.Consensus.Shelley.Ledger.Config
import           Ouroboros.Consensus.Shelley.Ledger.Integrity
import           Ouroboros.Consensus.Shelley.Ledger.Mempool
import           Ouroboros.Consensus.Shelley.Protocol
import           Ouroboros.Consensus.Shelley.Protocol.HotKey (HotKey)

{-------------------------------------------------------------------------------
  Forging
-------------------------------------------------------------------------------}

forgeShelleyBlock
  :: (TPraosCrypto c, Monad m)
  => HotKey c m
  -> TPraosCanBeLeader c
  -> TopLevelConfig (ShelleyBlock c)
  -> BlockNo                             -- ^ Current block number
  -> SlotNo                              -- ^ Current slot number
  -> TickedLedgerState (ShelleyBlock c)  -- ^ Current ledger
  -> [GenTx (ShelleyBlock c)]            -- ^ Txs to add in the block
  -> TPraosIsLeader c                    -- ^ Leader proof
  -> m (ShelleyBlock c)
forgeShelleyBlock hotKey canBeLeader cfg curNo curSlot tickedLedger txs isLeader = do
    tpraosFields <- forgeTPraosFields hotKey canBeLeader isLeader mkBhBody
    let blk = mkShelleyBlock $ SL.Block (mkHeader tpraosFields) body
    return $
      assert (verifyBlockIntegrity tpraosSlotsPerKESPeriod blk) $
      assertWithMsg bodySizeEstimate $
        blk
  where
    TPraosConfig { tpraosParams = TPraosParams { tpraosSlotsPerKESPeriod } } =
      configConsensus cfg

    body = SL.TxSeq $ Seq.fromList $ (\(ShelleyTx _ tx) -> tx) <$> txs

    mkHeader TPraosFields { tpraosSignature, tpraosToSign } =
      SL.BHeader tpraosToSign tpraosSignature

    prevHash =
        toShelleyPrevHash
      . castHash
      . getTipHash
      $ tickedLedger

    bodySizeEstimate :: Either String ()
    bodySizeEstimate
      | actualBodySize > estimatedBodySize + fixedBlockBodyOverhead
      = throwError $
          "Actual block body size > Estimated block body size + fixedBlockBodyOverhead: "
            <> show actualBodySize
            <> " > "
            <> show estimatedBodySize
            <> " + "
            <> show (fixedBlockBodyOverhead :: Int)
      | otherwise
      = return ()

    estimatedBodySize, actualBodySize :: Int
    estimatedBodySize = fromIntegral $ foldl' (+) 0 (map txInBlockSize txs)
    actualBodySize    = SL.bBodySize body

    mkBhBody toSign = SL.BHBody {
          bheaderPrev    = prevHash
        , bheaderVk      = SL.VKey tpraosToSignIssuerVK
        , bheaderVrfVk   = tpraosToSignVrfVK
        , bheaderSlotNo  = curSlot
        , bheaderBlockNo = curNo
        , bheaderEta     = tpraosToSignEta
        , bheaderL       = tpraosToSignLeader
        , bsize          = fromIntegral actualBodySize
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
