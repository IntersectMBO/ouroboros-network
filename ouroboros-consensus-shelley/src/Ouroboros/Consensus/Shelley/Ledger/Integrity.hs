{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
module Ouroboros.Consensus.Shelley.Ledger.Integrity (
    verifyHeaderIntegrity
  , verifyBlockIntegrity
  ) where

import           Data.Either (isRight)

import           Ouroboros.Consensus.Block

import qualified Shelley.Spec.Ledger.BlockChain as SL
import qualified Shelley.Spec.Ledger.Keys as SL
import qualified Shelley.Spec.Ledger.OCert as SL

import           Ouroboros.Consensus.Shelley.Ledger.Block
import           Ouroboros.Consensus.Shelley.Protocol

-- | Verify whether a header is not corrupted
verifyHeaderIntegrity :: TPraosCrypto c => Header (ShelleyBlock c) -> Bool
verifyHeaderIntegrity ShelleyHeader { shelleyHeaderRaw } =
    -- TODO uses evolution 0. I assume the generator simply doesn't ever
    -- evolve the KES key it signs with.
    isRight $ SL.verifySignedKES () ocertVkHot 0 hdrBody hdrSignature
  where
    SL.BHeader hdrBody hdrSignature = shelleyHeaderRaw
    SL.OCert { ocertVkHot } = SL.bheaderOCert hdrBody

-- | Verifies whether the block is not corrupted by checking its signature and
-- witnesses
verifyBlockIntegrity :: TPraosCrypto c => ShelleyBlock c -> Bool
verifyBlockIntegrity blk =
    verifyHeaderIntegrity (getHeader blk) &&
    blockMatchesHeader    (getHeader blk) blk
