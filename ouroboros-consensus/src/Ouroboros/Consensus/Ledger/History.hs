{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Template to track the /history/ of something in the ledger state.
--
-- For example, for Byron, this can be used to track the /delegation history/.
--
-- Intended for qualified import
module Ouroboros.Consensus.Ledger.History (
    History (..)
  , Snapshots
  , Snapshot
  , empty
  , snapOld
  , find
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad (guard)
import           Data.Coerce (coerce)
import           Data.Sequence.Strict (StrictSeq ((:<|), (:|>), Empty))
import           Data.Word (Word64)
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)

import           Cardano.Slotting.SlotBounded (Bounds (..), SlotBounded (..))
import qualified Cardano.Slotting.SlotBounded as SB

import           Ouroboros.Network.Block (SlotNo (..))
import           Ouroboros.Network.Point (WithOrigin (..))

import           Ouroboros.Consensus.Util (firstJust)

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

-- | Empty (genesis) history
empty :: History st
empty = History Origin Empty

-- | Take a snapshot
snapOld :: forall st.
           Word64  -- ^ Maximum rollback (@k@)
        -> SlotNo  -- ^ Slot number of the block that changed the state
        -> st      -- ^ State /before/ it changed
        -> History st -> History st
snapOld k now old = trim k now . go
  where
    go :: History st -> History st
    go h@History{..} = History {
          historyAnchor    = historyAnchor
        , historySnapshots = historySnapshots
                         :|> SB.bounded (lowerBound h) now old
        }

    -- Compute the (inclusive) lower bound for this snapshot
    --
    -- Must be equal to the (exclusive) upper bound for the preceding snapshot;
    -- if the history is empty, we use its anchor instead.
    lowerBound :: History st -> WithOrigin SlotNo
    lowerBound History{..} =
        case historySnapshots of
          Empty     -> historyAnchor
          (_ :|> s) -> At (sbUpper s)

-- | Drop snapshots guaranteed not to be needed anymore
--
-- Implementation note: the snapshots look something like
--
-- > [......)[...............)[....)
-- >             ^                      ^
-- >            earliest               now
--
-- We process them from old to now. Any old snapshots that do not contain
-- @earliest@ are removed,  and stop as soon we find the first snapshot that
-- /does/ contain@earliest@.
--
-- We always leave at least one snapshot in the list (see 'History').
trim :: forall st.
        Word64  -- ^ Maximum rollback (@k@)
     -> SlotNo  -- ^ Current slot
     -> History st -> History st
trim k now h@History{..} =
    case trimSeq historySnapshots of
      Nothing      -> h
      Just (s, ss) -> History (At (sbUpper s)) ss
  where
    -- Earliest slot we might roll back to
    earliest :: WithOrigin SlotNo
    earliest = if now >= (2 * coerce k)
                 then At $ now - (2 * coerce k)
                 else Origin

    -- Trim the list of snapshots, if we can.
    -- Returns the most recent snapshot we dropped
    trimSeq :: Snapshots st -> Maybe (Snapshot st, Snapshots st)
    trimSeq Empty      = Nothing
    trimSeq (s :<| ss) = do guard (not (s `SB.contains` earliest))
                            trimSeq ss <|> Just (s, ss)

find :: WithOrigin SlotNo -> History st -> Maybe st
find slot = firstJust (`SB.at` slot) . historySnapshots
