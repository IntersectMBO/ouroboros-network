module Ouroboros.Consensus.Shelley.Ledger.Integrity (
    verifyBlockIntegrity
  , verifyHeaderIntegrity
  ) where

import           Data.Word (Word64)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Shelley.Ledger.Block
import           Ouroboros.Consensus.Shelley.Protocol.Abstract
                     (verifyHeaderIntegrity)

-- | Verifies whether the block is not corrupted by checking its signature and
-- witnesses.
verifyBlockIntegrity ::
     ShelleyCompatible proto era
  => Word64
  -> ShelleyBlock proto era -> Bool
verifyBlockIntegrity spkp blk =
    verifyHeaderIntegrity spkp (shelleyHeaderRaw $ getHeader blk) &&
    blockMatchesHeader (getHeader blk) blk
