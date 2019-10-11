{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Store the history of the Shelley ledger view in order to allow time-travel
-- to the recent past.
module Ouroboros.Consensus.Ledger.Shelley.History
  ( LedgerViewHistory
  , Snapshot
  , empty
  , snapOld
  , trim
  , find
  )
where

import qualified Cardano.Ledger.Shelley.API as Shelley
import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Cardano.Slotting.Slot (SlotNo (..), WithOrigin, fromWithOrigin,
                     genesisSlotNo)
import           Cardano.Slotting.SlotBounded (Bounds (..), SlotBounded (..))
import qualified Cardano.Slotting.SlotBounded as SB
import           Data.Coerce (coerce)
import           Data.Sequence.Strict (StrictSeq ((:<|), (:|>), Empty))
import qualified Data.Sequence.Strict as Seq
import           Data.Word (Word64)
import           Ouroboros.Consensus.Protocol.TPraos (TPraosStandardCrypto)
import           Ouroboros.Consensus.Util (firstJust)

type LedgerView = Shelley.LedgerView TPraosStandardCrypto

newtype LedgerViewHistory = LedgerViewHistory
  { unLedgerViewHistory :: StrictSeq Snapshot}
  deriving (Show, Eq, NoUnexpectedThunks)

-- | Historical snapshot of the delegation state
--
-- See 'LedgerViewHistory' for details
type Snapshot = SlotBounded IX LedgerView

-- | Empty (genesis) delegation history
--
-- The delegation history should only be empty if it has never changed.
empty :: LedgerViewHistory
empty = LedgerViewHistory Seq.empty

-- | Take a snapshot of the delegation state
snapOld :: Word64  -- ^ Maximum rollback (@k@)
        -> SlotNo         -- ^ Slot number of the block that changed delegation
        -> LedgerView -- ^ Delegation state /before/ it changed
        -> LedgerViewHistory -> LedgerViewHistory
snapOld k now old = trim k now . coerce append
  where
    append :: StrictSeq Snapshot -> StrictSeq Snapshot
    append ss = ss :|> SB.bounded (lowerBound ss) now old

    lowerBound :: StrictSeq Snapshot -> SlotNo
    lowerBound Empty     = genesisSlotNo
    lowerBound (_ :|> s) = sbUpper s

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
-- We always leave at least one snapshot in the list (see 'LedgerViewHistory').
trim :: Word64  -- ^ Maximum rollback (@k@)
     -> SlotNo         -- ^ Current slot
     -> LedgerViewHistory -> LedgerViewHistory
trim k now = coerce go
  where
    go :: StrictSeq Snapshot -> StrictSeq Snapshot
    go Empty         = Empty
    go (s :<| Empty) = s :<| Empty
    go (s :<| ss)    = if s `SB.contains` earliest
                         then s :<| ss
                         else go ss

    -- Earliest slot we might roll back to
    earliest :: SlotNo
    earliest = now - 2 * coerce k

find :: WithOrigin SlotNo -> LedgerViewHistory -> Maybe LedgerView
find slot (LedgerViewHistory history) =
    firstJust (`SB.at` slot') history
  where
    slot' :: SlotNo
    slot' = fromWithOrigin genesisSlotNo slot
