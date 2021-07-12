{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}

module Ouroboros.Consensus.Shelley.Ledger.Forge (forgeShelleyBlock) where

import           Control.Exception
import           Control.Monad.Except
import           Data.List (foldl')
import qualified Data.Sequence.Strict as Seq

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Util.Assert

import qualified Cardano.Ledger.Core as Core (Tx)
import qualified Cardano.Ledger.Era as SL (hashTxSeq, toTxSeq)
import qualified Shelley.Spec.Ledger.API as SL (extractTx)
import qualified Shelley.Spec.Ledger.BlockChain as SL

import           Ouroboros.Consensus.Mempool.TxLimits (TxLimits)
import qualified Ouroboros.Consensus.Mempool.TxLimits as TxLimits
import           Ouroboros.Consensus.Shelley.Eras (EraCrypto)
import           Ouroboros.Consensus.Shelley.Ledger.Block
import           Ouroboros.Consensus.Shelley.Ledger.Config
import           Ouroboros.Consensus.Shelley.Ledger.Integrity
import           Ouroboros.Consensus.Shelley.Ledger.Mempool
import           Ouroboros.Consensus.Shelley.Protocol
import           Ouroboros.Consensus.Shelley.Protocol.HotKey (HotKey)

{-------------------------------------------------------------------------------
  Forging
-------------------------------------------------------------------------------}

forgeShelleyBlock ::
     forall m era. (ShelleyBasedEra era, TxLimits (ShelleyBlock era), Monad m)
  => HotKey (EraCrypto era) m
  -> TPraosCanBeLeader (EraCrypto era)
  -> TopLevelConfig (ShelleyBlock era)
  -> TxLimits.Overrides (ShelleyBlock era)     -- ^ How to override max tx
                                               -- capacity defined by ledger
  -> BlockNo                                   -- ^ Current block number
  -> SlotNo                                    -- ^ Current slot number
  -> TickedLedgerState (ShelleyBlock era)      -- ^ Current ledger
  -> [Validated (GenTx (ShelleyBlock era))]    -- ^ Txs to add in the block
  -> TPraosIsLeader (EraCrypto era)            -- ^ Leader proof
  -> m (ShelleyBlock era)
forgeShelleyBlock hotKey canBeLeader cfg maxTxCapacityOverrides curNo curSlot tickedLedger txs isLeader = do
    tpraosFields <- forgeTPraosFields hotKey canBeLeader isLeader mkBhBody
    let blk = mkShelleyBlock $ SL.Block (mkHeader tpraosFields) body
    return $
      assert (verifyBlockIntegrity tpraosSlotsPerKESPeriod blk) $
      assertWithMsg bodySizeEstimate $
        blk
  where
    TPraosConfig { tpraosParams = TPraosParams { tpraosSlotsPerKESPeriod } } =
      configConsensus cfg

    body =
        SL.toTxSeq @era
      . Seq.fromList
      . fmap extractTx
      $ takeLargestPrefixThatFits computedMaxTxCapacity txs

    computedMaxTxCapacity = computeMaxTxCapacity tickedLedger maxTxCapacityOverrides

    extractTx :: (Validated (GenTx (ShelleyBlock era))) -> Core.Tx era
    extractTx (ShelleyValidatedTx _txid vtx) = SL.extractTx vtx

    mkHeader TPraosFields { tpraosSignature, tpraosToSign } =
      SL.BHeader tpraosToSign tpraosSignature

    prevHash :: SL.PrevHash (EraCrypto era)
    prevHash =
        toShelleyPrevHash @era
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
    estimatedBodySize = fromIntegral $ foldl' (+) 0 $ map (txInBlockSize . txForgetValidated) txs
    actualBodySize    = SL.bBodySize body

    mkBhBody toSign = SL.BHBody {
          bheaderPrev    = prevHash
        , bheaderVk      = tpraosToSignIssuerVK
        , bheaderVrfVk   = tpraosToSignVrfVK
        , bheaderSlotNo  = curSlot
        , bheaderBlockNo = curNo
        , bheaderEta     = tpraosToSignEta
        , bheaderL       = tpraosToSignLeader
        , bsize          = fromIntegral actualBodySize
        , bhash          = SL.hashTxSeq @era body
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
