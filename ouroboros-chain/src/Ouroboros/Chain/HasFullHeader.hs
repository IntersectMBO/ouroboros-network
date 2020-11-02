module Ouroboros.Chain.HasFullHeader (
    HasFullHeader (..)
  ) where

import           Ouroboros.Chain.HasHeader

-- | Extension of 'HasHeader' with some additional information
--
-- Used in tests and assertions only.
class HasHeader b => HasFullHeader b where
    blockPrevHash  :: b -> ChainHash b
    blockInvariant :: b -> Bool
