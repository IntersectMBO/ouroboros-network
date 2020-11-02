module Ouroboros.Chain.Size (
    SizeInBytes
  , TxSizeInBytes
  ) where

import           Data.Word (Word32)

type SizeInBytes = Word32

-- | Transactions are typically not big, but in principle in future we could
-- have ones over 64k large.
--
type TxSizeInBytes = Word32
