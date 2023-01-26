{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
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
import qualified Cardano.Ledger.Shelley.API as SL (Block (..), extractTx)
import qualified Cardano.Ledger.Shelley.BlockChain as SL (bBodySize)
import qualified Cardano.Protocol.TPraos.BHeader as SL

import           Ouroboros.Consensus.Mempool.TxLimits (TxLimits)
import qualified Ouroboros.Consensus.Mempool.TxLimits as TxLimits
import           Ouroboros.Consensus.Protocol.Abstract (CanBeLeader, IsLeader)
import           Ouroboros.Consensus.Protocol.Ledger.HotKey (HotKey)
import           Ouroboros.Consensus.Shelley.Eras (EraCrypto)
import           Ouroboros.Consensus.Shelley.Ledger.Block
import           Ouroboros.Consensus.Shelley.Ledger.Config
                     (shelleyProtocolVersion)
import           Ouroboros.Consensus.Shelley.Ledger.Integrity
import           Ouroboros.Consensus.Shelley.Ledger.Mempool
import           Ouroboros.Consensus.Shelley.Protocol.Abstract (ProtoCrypto,
                     ProtocolHeaderSupportsKES (configSlotsPerKESPeriod),
                     mkHeader)

{-------------------------------------------------------------------------------
  Forging
-------------------------------------------------------------------------------}

forgeShelleyBlock ::
     forall m era proto.
      (ShelleyCompatible proto era, TxLimits (ShelleyBlock proto era), Monad m)
  => HotKey (EraCrypto era) m
  -> CanBeLeader proto
  -> TopLevelConfig (ShelleyBlock proto era)
  -> TxLimits.Overrides (ShelleyBlock proto era)          -- ^ How to override max tx
                                                          --   capacity defined by ledger
  -> BlockNo                                              -- ^ Current block number
  -> SlotNo                                               -- ^ Current slot number
  -> TickedLedgerState (ShelleyBlock proto era) Canonical -- ^ Current ledger
  -> [Validated (GenTx (ShelleyBlock proto era))]         -- ^ Txs to add in the block
  -> IsLeader proto
  -> m (ShelleyBlock proto era)
forgeShelleyBlock
  hotKey
  cbl
  cfg
  maxTxCapacityOverrides
  curNo
  curSlot
  tickedLedger
  txs
  isLeader = do
    hdr <- mkHeader @_ @(ProtoCrypto proto) hotKey cbl isLeader
      curSlot curNo prevHash (SL.hashTxSeq @era body) actualBodySize (shelleyProtocolVersion $ configBlock cfg)
    let blk = mkShelleyBlock $ SL.Block hdr body
    return $
      assert (verifyBlockIntegrity (configSlotsPerKESPeriod $ configConsensus cfg) blk) $
      assertWithMsg bodySizeEstimate blk
  where

    body =
        SL.toTxSeq @era
      . Seq.fromList
      . fmap extractTx
      $ takeLargestPrefixThatFits maxTxCapacityOverrides tickedLedger txs

    extractTx :: Validated (GenTx (ShelleyBlock proto era)) -> Core.Tx era
    extractTx (ShelleyValidatedTx _txid vtx) = SL.extractTx vtx

    prevHash :: SL.PrevHash (EraCrypto era)
    prevHash =
        toShelleyPrevHash @era @proto
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
