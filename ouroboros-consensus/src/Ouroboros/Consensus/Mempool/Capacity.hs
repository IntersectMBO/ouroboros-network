{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

-- | Mempool capacity, size and transaction size datatypes.
--
-- This module also defines how to manually override the mempool capacity.
--
-- > import           Ouroboros.Consensus.Mempool.Capacity (Capacity)
-- > import qualified Ouroboros.Consensus.Mempool.Capacity as Capacity
module Ouroboros.Consensus.Mempool.Capacity (
    -- * Mempool capacity
    MempoolCapacityBytes (..)
  , MempoolCapacityBytesOverride (..)
  , computeMempoolCapacity
    -- * Mempool Size
  , MempoolSize (..)
    -- * Transaction size
  , ByteSize (..)
  , TxLimits (..)
  , (<=)
    -- * Restricting more strongly than the ledger's limits
  , TxOverrides
  , applyOverrides
  , getOverrides
  , mkOverrides
  , noOverridesMeasure
  ) where

import           Prelude hiding ((<=))

import           Data.Coerce (coerce)
import           Data.Word (Word32)
import           NoThunks.Class

import           Data.Measure (BoundedMeasure, Measure)
import qualified Data.Measure as Measure

import           Ouroboros.Consensus.Ledger.Basics
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Ticked (Ticked (..))

{-------------------------------------------------------------------------------
  Mempool capacity in bytes
-------------------------------------------------------------------------------}

-- | Represents the maximum number of bytes worth of transactions that a
-- 'Mempool' can contain.
newtype MempoolCapacityBytes = MempoolCapacityBytes {
      getMempoolCapacityBytes :: Word32
    }
  deriving (Eq, Show, NoThunks)

-- | An override for the default 'MempoolCapacityBytes' which is 2x the
-- maximum transaction capacity
data MempoolCapacityBytesOverride
  = NoMempoolCapacityBytesOverride
    -- ^ Use 2x the maximum transaction capacity of a block. This will change
    -- dynamically with the protocol parameters adopted in the current ledger.
  | MempoolCapacityBytesOverride !MempoolCapacityBytes
    -- ^ Use the following 'MempoolCapacityBytes'.
  deriving (Eq, Show)


-- | If no override is provided, calculate the default mempool capacity as 2x
-- the current ledger's maximum transaction capacity of a block.
computeMempoolCapacity
  :: LedgerSupportsMempool blk
  => TickedLedgerState blk
  -> MempoolCapacityBytesOverride
  -> MempoolCapacityBytes
computeMempoolCapacity st mc = case mc of
    NoMempoolCapacityBytesOverride        -> noOverride
    MempoolCapacityBytesOverride override -> override
  where
    noOverride = MempoolCapacityBytes (txsMaxBytes st * 2)

{-------------------------------------------------------------------------------
  Mempool size
-------------------------------------------------------------------------------}

-- | The size of a mempool.
data MempoolSize = MempoolSize
  { msNumTxs   :: !Word32
    -- ^ The number of transactions in the mempool.
  , msNumBytes :: !Word32
    -- ^ The summed byte size of all the transactions in the mempool.
  } deriving (Eq, Show)

instance Semigroup MempoolSize where
  MempoolSize xt xb <> MempoolSize yt yb = MempoolSize (xt + yt) (xb + yb)

instance Monoid MempoolSize where
  mempty = MempoolSize { msNumTxs = 0, msNumBytes = 0 }
  mappend = (<>)

{-------------------------------------------------------------------------------
  Tx sizes
-------------------------------------------------------------------------------}

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
class BoundedMeasure (TxMeasure blk) => TxLimits blk where
  type TxMeasure blk

  -- | What is the measure an individual tx?
  txMeasure        :: Validated (GenTx blk)    -> TxMeasure blk

  -- | What is the allowed capacity for txs in an individual block?
  txsBlockCapacity :: Ticked (LedgerState blk) -> TxMeasure blk

-- | Is every component of the first value less-than-or-equal-to the
-- corresponding component of the second value?
(<=) :: Measure a => a -> a -> Bool
(<=) = (Measure.<=)

{-------------------------------------------------------------------------------
  ByteSize
-------------------------------------------------------------------------------}

newtype ByteSize = ByteSize { unByteSize :: Word32 }
  deriving stock (Show)
  deriving newtype (Eq, Ord)
  deriving newtype (BoundedMeasure, Measure)

{-------------------------------------------------------------------------------
  Overrides
-------------------------------------------------------------------------------}

-- | An override that lowers a capacity limit
--
-- Specifically, we use this override to let the node operator limit the total
-- 'TxMeasure' of transactions in blocks even more severely than would the
-- ledger state's 'txsBlockCapacity'. The forge logic will use the 'Measure.min'
-- (ie the lattice's @meet@ operator) to combine this override with the capacity
-- given by the ledger state. More concretely, that will typically be a
-- componentwise minimum operation, along each of the components\/dimensions of
-- @'TxMeasure' blk@.
--
-- This newtype wrapper distinguishes the intention of this particular
-- 'TxMeasure' as such an override. We use 'TxMeasure' in different ways in this
-- code base. The newtype also allows us to distinguish the one most appropriate
-- monoid among many offered by the 'TxLimits' superclass constraints: it is the
-- monoid induced by the bounded meet-semilattice (see 'BoundedMeasure') that is
-- relevant to the notion of /overriding/ the ledger's block capacity.
newtype TxOverrides blk =
  -- This constructor is not exported.
  TxOverrides { getOverrides :: TxMeasure blk }

instance TxLimits blk => Monoid (TxOverrides blk) where
  mempty = TxOverrides noOverridesMeasure

instance TxLimits blk => Semigroup (TxOverrides blk) where
  (<>) = coerce $ Measure.min @(TxMeasure blk)

-- | @'applyOverrides' 'noOverrides' m = m@
noOverridesMeasure :: BoundedMeasure a => a
noOverridesMeasure = Measure.maxBound

-- | Smart constructor for 'Overrides'.
mkOverrides :: TxMeasure blk -> TxOverrides blk
mkOverrides = TxOverrides

-- | Apply the override
applyOverrides ::
     TxLimits blk
  => TxOverrides blk
  -> TxMeasure blk
  -> TxMeasure blk
applyOverrides (TxOverrides m') m = Measure.min m' m
