{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Limits on the ledger-specific _measure_ (eg size) of a sequence of
-- transactions
--
-- > import           Ouroboros.Consensus.Mempool.TxLimits (TxLimits)
-- > import qualified Ouroboros.Consensus.Mempool.TxLimits as TxLimits
module Ouroboros.Consensus.Mempool.TxLimits (
    ByteSize (..)
  , Overrides
  , TxLimits (..)
  , applyOverrides
  , mkOverrides
  , noOverrides
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

  lessEq       :: Measure blk -> Measure blk -> Bool
  txMeasure    :: Validated (GenTx blk) -> Measure blk
  maxCapacity  :: Ticked (LedgerState blk) -> Measure blk
  pointwiseMin :: Measure blk -> Measure blk -> Measure blk

newtype ByteSize = ByteSize { unByteSize :: Word32 }
  deriving stock (Show, Eq, Ord)

instance Semigroup ByteSize where
  (ByteSize bs1) <> (ByteSize bs2) = ByteSize $ bs1 + bs2

instance Monoid ByteSize where
  mempty = ByteSize 0

-- | How to override the limits set by the ledger state
--
-- The forge logic must use the 'pointwiseMin'imum of the limits from ledger
-- state and from the result of this override.
newtype Overrides blk = Overrides (Measure blk -> Measure blk)

-- | Do not alter the limits set by the ledger state
noOverrides :: Overrides blk
noOverrides = Overrides id

mkOverrides :: (Measure blk -> Measure blk) -> Overrides blk
mkOverrides = Overrides

-- | Apply the override function and then take the pointwise minimum
applyOverrides :: forall blk.
     TxLimits blk
  => Overrides blk
  -> Measure blk
  -> Measure blk
applyOverrides (Overrides f) m = pointwiseMin @blk m (f m)
