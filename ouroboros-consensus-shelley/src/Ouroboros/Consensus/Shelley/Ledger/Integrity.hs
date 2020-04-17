{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
module Ouroboros.Consensus.Shelley.Ledger.Integrity (
    verifyBlockMatchesHeader
  , verifyHeaderIntegrity
  , verifyBlockIntegrity
  ) where

import           Data.Word (Word64)

import           Ouroboros.Network.Block (SlotNo (..), blockSlot)

import           Ouroboros.Consensus.Block

import qualified Shelley.Spec.Ledger.BlockChain as SL
import qualified Shelley.Spec.Ledger.Keys as SL
import qualified Shelley.Spec.Ledger.OCert as SL

import           Ouroboros.Consensus.Shelley.Ledger.Block
import           Ouroboros.Consensus.Shelley.Protocol

-- | Check if a block matches its header by checking whether the recomputed
-- hash of the body matches the hash of the body stored in the header.
verifyBlockMatchesHeader
  :: Crypto c
  => Header (ShelleyBlock c) -> ShelleyBlock c -> Bool
verifyBlockMatchesHeader hdr blk =
    -- Compute the hash the body of the block (the transactions) and compare
    -- that against the hash of the body stored in the header.
    SL.bbHash txs == SL.bhash hdrBody
  where
    ShelleyHeader { shelleyHeaderRaw = SL.BHeader hdrBody _ } = hdr
    ShelleyBlock  { shelleyBlockRaw  = SL.Block _ txs }       = blk

-- | Verify whether a header is not corrupted
verifyHeaderIntegrity
  :: TPraosCrypto c
  => Word64  -- ^ 'tpraosSlotsPerKESPeriod'
  -> Header (ShelleyBlock c) -> Bool
verifyHeaderIntegrity slotsPerKESPeriod hdr@ShelleyHeader { shelleyHeaderRaw } =
    SL.verifyKES ocertVkHot hdrBody hdrSignature t
  where
    SL.BHeader hdrBody hdrSignature = shelleyHeaderRaw
    SL.OCert {
        ocertVkHot
      , ocertKESPeriod = SL.KESPeriod startOfKesPeriod
      } = SL.bheaderOCert hdrBody

    currentKesPeriod = fromIntegral $
      unSlotNo (blockSlot hdr) `div` slotsPerKESPeriod

    t | currentKesPeriod >= startOfKesPeriod
      = currentKesPeriod - startOfKesPeriod
      | otherwise
      = 0

-- | Verifies whether the block is not corrupted by checking its signature and
-- witnesses
verifyBlockIntegrity
  :: TPraosCrypto c
  => Word64  -- ^ 'tpraosSlotsPerKESPeriod'
  -> ShelleyBlock c -> Bool
verifyBlockIntegrity slotsPerKESPeriod blk =
    verifyHeaderIntegrity slotsPerKESPeriod (getHeader blk) &&
    verifyBlockMatchesHeader                (getHeader blk) blk
