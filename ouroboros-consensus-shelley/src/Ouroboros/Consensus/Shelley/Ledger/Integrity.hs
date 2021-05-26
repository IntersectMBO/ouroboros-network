{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE TypeFamilies             #-}
module Ouroboros.Consensus.Shelley.Ledger.Integrity (
    verifyBlockIntegrity
  , verifyHeaderIntegrity
  ) where

import           Data.Either (isRight)
import           Data.Word (Word64)

import           Ouroboros.Consensus.Block

import qualified Cardano.Ledger.Keys as SL (verifySignedKES)
import qualified Shelley.Spec.Ledger.API as SL

import           Ouroboros.Consensus.Shelley.Ledger.Block

-- | Verify whether a header is not corrupted
verifyHeaderIntegrity ::
     ShelleyBasedEra era
  => Word64  -- ^ 'tpraosSlotsPerKESPeriod'
  -> Header (ShelleyBlock era)
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
verifyBlockIntegrity ::
     ShelleyBasedEra era
  => Word64  -- ^ 'tpraosSlotsPerKESPeriod'
  -> ShelleyBlock era -> Bool
verifyBlockIntegrity slotsPerKESPeriod blk =
    verifyHeaderIntegrity slotsPerKESPeriod (getHeader blk) &&
    blockMatchesHeader                      (getHeader blk) blk
