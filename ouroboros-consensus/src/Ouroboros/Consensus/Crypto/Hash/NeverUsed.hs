module Ouroboros.Consensus.Crypto.Hash.NeverUsed (NeverHash) where

import           Ouroboros.Consensus.Crypto.Hash.Class

-- | HASH never used
--
-- Will throw a runtime exception when trying to hash something.
data NeverHash

instance HashAlgorithm NeverHash where
  byteCount _ = 0
  digest = error "HASH not available"
