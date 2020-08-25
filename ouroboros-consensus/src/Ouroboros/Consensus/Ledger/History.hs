{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Template to track the /history/ of something in the ledger state.
--
-- For example, for Byron, this can be used to track the /delegation history/.
--
-- DEPRECATED as of #1935 but kept around to allow backwards compatible
-- decoders.
--
-- Intended for qualified import
module Ouroboros.Consensus.Ledger.History (
    History (..)
  , Snapshots
  , Snapshot
  ) where

import           Data.Sequence.Strict (StrictSeq)
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)

import           Cardano.Slotting.SlotBounded (Bounds (..), SlotBounded (..))

import           Ouroboros.Consensus.Block

{-------------------------------------------------------------------------------
  History
-------------------------------------------------------------------------------}

-- | History
--
-- In some ledgers, it is required to store some historical snapshots of some
-- piece of data to support the implementation of
-- 'ledgerViewForecastAt'. This data type is a template to track
-- some form of /state/ that requires historical snapshots in the ledger
-- state. If the description below fits the bill, you can use the 'History'
-- type to track your /state/:
--
-- Each time that the state is updated (that is, when applying a block that
-- changes it), we take a snapshot of the /old/ state (the state as it was
-- before the block was applied).
--
-- We store the state along with its slot bounds:
--
-- * The slot number of the block that changed the state serves as an
--   /exclusive/ upper bound.
-- * The (exclusive) upper bound of the /previous/ historical state serves as
--   an /inclusive/ lower bound (since this is the slot at which this state
--   became active).
--
-- We never need to go back in history for more than @2k@ slots, allowing us
-- to drop states from history as time passes, keeping the history bounded in
-- size. The history will be empty if
--
-- * The state never changed, or
-- * We cannot roll back far enough to require a historical state.
--
-- Since the (inclusive) lower bound of the next snapshot will be set to be
-- equal to the (exclusive) upper bound of the previous, we must remember this
-- previous bound even when the ledger is empty. Near genesis we will set this
-- at @Origin@, which will indeed by the lower bound for the first snapshot.
data History st = History {
      historyAnchor    :: !(WithOrigin SlotNo)
    , historySnapshots :: !(Snapshots st)
    }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NoUnexpectedThunks)

-- | Snapshots strictly after genesis
--
-- More recent snapshots are stored at the end of the sequence.
--
-- Invariant: the (exclusive) upper bound of each snapshot must equal the
-- (inclusive) lower bound of the next.
type Snapshots st = StrictSeq (Snapshot st)

-- | Historical snapshot of @s@
--
-- See 'History' for details
type Snapshot st = SlotBounded 'IX st
