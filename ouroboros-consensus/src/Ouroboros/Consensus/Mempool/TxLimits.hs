{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

-- | Limits on the ledger-specific _measure_ (eg size) of a sequence of
-- transactions
--
-- > import           Ouroboros.Consensus.Mempool.TxLimits (TxLimits)
-- > import qualified Ouroboros.Consensus.Mempool.TxLimits as TxLimits
module Ouroboros.Consensus.Mempool.TxLimits (
    ByteSize (..)
  , TxLimits (..)
  , (Ouroboros.Consensus.Mempool.TxLimits.<=)
    -- * Restricting more strongly than the ledger's limits
  , Overrides
  , applyOverrides
  , getOverrides
  , mkOverrides
  , noOverridesMeasure
  ) where

import           Data.Coerce (coerce)
import           Data.Word (Word32)

import           Data.Measure (BoundedMeasure, Measure)
import qualified Data.Measure as Measure

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
newtype Overrides blk =
  -- This constructor is not exported.
  Overrides {getOverrides :: TxMeasure blk}

instance TxLimits blk => Monoid (Overrides blk) where
  mempty = Overrides noOverridesMeasure

instance TxLimits blk => Semigroup (Overrides blk) where
  (<>) = coerce $ Measure.min @(TxMeasure blk)

-- | @'applyOverrides' 'noOverrides' m = m@
noOverridesMeasure :: BoundedMeasure a => a
noOverridesMeasure = Measure.maxBound

-- | Smart constructor for 'Overrides'.
mkOverrides :: TxMeasure blk -> Overrides blk
mkOverrides = Overrides

-- | Apply the override
applyOverrides ::
     TxLimits blk
  => Overrides blk
  -> TxMeasure blk
  -> TxMeasure blk
applyOverrides (Overrides m') m = Measure.min m' m
