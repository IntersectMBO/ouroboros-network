{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
module Ouroboros.Consensus.Shelley.Ledger.Integrity (
    verifyHeaderIntegrity
  , verifyBlockIntegrity
  ) where

import           Data.Either (isRight)
import           Data.Word (Word64)

import           Ouroboros.Consensus.Block

import qualified Shelley.Spec.Ledger.BlockChain as SL
import qualified Shelley.Spec.Ledger.Keys as SL
import qualified Shelley.Spec.Ledger.OCert as SL

import           Ouroboros.Consensus.Shelley.Ledger.Block
import           Ouroboros.Consensus.Shelley.Protocol

-- | Verify whether a header is not corrupted
verifyHeaderIntegrity
  :: TPraosCrypto c
  => Word64  -- ^ 'tpraosSlotsPerKESPeriod'
  -> Header (ShelleyBlock c)
  -> Bool
verifyHeaderIntegrity slotsPerKESPeriod hdr@ShelleyHeader { shelleyHeaderRaw } =
    isRight $ SL.verifySignedKES () ocertVkHot t hdrBody hdrSignature
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
    blockMatchesHeader                      (getHeader blk) blk
