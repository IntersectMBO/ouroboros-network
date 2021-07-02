{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Ouroboros.Consensus.Mempool.TxLimits (
    ByteSize (..)
  , TxLimits (..)
  ) where

import           Data.Word (Word32)

import           Ouroboros.Consensus.Ledger.Abstract (Validated)
import           Ouroboros.Consensus.Ledger.Basics (LedgerState)
import           Ouroboros.Consensus.Ledger.SupportsMempool (GenTx)
import           Ouroboros.Consensus.Ticked (Ticked (..))

-- | Each block has its limits of how many transactions it can hold.
-- That limit is compared against the sum of measurements
-- taken of each of the transactions in that block.
--
-- How we measure the transaction depends of the era that this
-- transaction belongs to (more specifically it depends on the block
-- type to which this transaction will be added). For initial eras
-- (like Byron and initial generations of Shelley based eras) this
-- measure was simply a ByteSize (block could not be bigger then
-- given size - in bytes - specified by the ledger state). In future
-- eras (starting with Alonzo) this measure was a bit more complex
-- as it had to take other factors into account (like execution units).
-- For details please see the individual instances for the TxLimits.
class (Monoid (Measure blk)) => TxLimits blk where

  type Measure blk

  lessEq      :: Measure blk -> Measure blk -> Bool
  txMeasure   :: Validated (GenTx blk) -> Measure blk
  maxCapacity :: Ticked (LedgerState blk) -> Measure blk

newtype ByteSize = ByteSize { unByteSize :: Word32 }
  deriving stock (Show, Eq, Ord)
  deriving newtype Num

instance Semigroup ByteSize where
  (ByteSize bs1) <> (ByteSize bs2) = ByteSize $ bs1 + bs2

instance Monoid ByteSize where
  mempty = ByteSize 0
