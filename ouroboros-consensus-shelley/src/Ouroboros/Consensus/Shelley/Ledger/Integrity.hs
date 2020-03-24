module Ouroboros.Consensus.Shelley.Ledger.Integrity (
    verifyBlockMatchesHeader
  , verifyHeaderIntegrity
  , verifyBlockIntegrity
  ) where

import           Ouroboros.Consensus.Block

import qualified Shelley.Spec.Ledger.BlockChain as SL

import           Ouroboros.Consensus.Shelley.Ledger.Block

-- | Check if a block matches its header
verifyBlockMatchesHeader
  :: Crypto c
  => Header (ShelleyBlock c) -> ShelleyBlock c -> Bool
verifyBlockMatchesHeader (ShelleyHeader header _)
                         (ShelleyBlock (SL.Block header' _) _) =
    -- TODO best approach
    header == header'

-- | Verify whether a header matches is not corrupted
verifyHeaderIntegrity :: Header (ShelleyBlock c) -> Bool
verifyHeaderIntegrity = const True -- TODO #1821

-- | Verifies whether the block is not corrupted by checking its signature and
-- witnesses
verifyBlockIntegrity :: ShelleyBlock c -> Bool
verifyBlockIntegrity = const True -- TODO #1821
