{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Delegation history
--
-- Intended for qualified import
module Ouroboros.Consensus.Ledger.Byron.DelegationHistory (
    DelegationHistory
  , empty
  , toSequence
  , snapOld
  , find
    -- * Serialisation
  , encodeDelegationHistory
  , decodeDelegationHistory
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Codec.Serialise (decode, encode)
import           Data.Coerce
import qualified Data.Foldable as Foldable
import           Data.Sequence.Strict (StrictSeq ((:<|), (:|>), Empty))
import qualified Data.Sequence.Strict as Seq

import           Cardano.Prelude (NoUnexpectedThunks)

import qualified Cardano.Chain.Common as CC
import qualified Cardano.Chain.Delegation as Delegation

import           Ouroboros.Network.Block (SlotNo (..), genesisSlotNo)
import           Ouroboros.Network.Point (WithOrigin (..), fromWithOrigin)

import           Ouroboros.Consensus.Ledger.Byron.PBFT
import           Ouroboros.Consensus.Protocol.PBFT
import           Ouroboros.Consensus.Util.SlotBounded (Bounds (..),
                     SlotBounded (..))
import qualified Ouroboros.Consensus.Util.SlotBounded as SB

{-------------------------------------------------------------------------------
  Delegation history
-------------------------------------------------------------------------------}

-- | Delegation history
--
-- Motivation: the ledger state gives us both the current delegation state
-- ('getDelegationMap') as well any planned future changes to the delegation
-- state ('getScheduledDelegations'). It does not however give us information
-- about past delegation states. This is where the 'DelegationHistory' comes in.
--
-- Each time that the delegation state is updated (that is, when applying a
-- block that changes the delegation state), we take a snapshot of the /old/
-- delegation state (the delegation state as it was before the block was
-- applied).
--
-- We store the delegation state state along with its slot bounds:
--
-- * The slot number of the block that changed the delegation state serves as an
--   /exclusive/ upper bound.
-- * The (exclusive) upper bound of the /previous/ historical delegation state
--   serves as an /inclusive/ lower bound (since this is the slot at which this
--   delegation state became active).
--
-- We never need to go back in history for more than @2k@ slots, allowing us
-- to drop delegation states from history as time passes, keeping the history
-- bounded in size. We will however always keep at least /one/ value, so that
-- we correctly compute the lower bound when we take a snapshot. (Alternatively,
-- we could set "good enough" lower bounds based on the @2k@ limit, but this
-- design is easier to understand and verify).
--
-- The delegation history will only be empty if delegation has never changed;
-- in this case, the first snapshot we add must be the genesis delegation and
-- so its lower bound will be 'genesisSlotNo'.
newtype DelegationHistory = DelegationHistory {
      -- | Sequence of historical snapshots (see above)
      --
      -- More recent snapshots are stored at the end of the sequence.
      --
      -- Invariant: the (exclusive) upper bound of each snapshot must equal the
      -- (inclusive) lower bound of the next.
      toSequence :: StrictSeq Snapshot
    }
  deriving (Show, Eq, NoUnexpectedThunks)

-- | Historical snapshot of the delegation state
--
-- See 'DelegationHistory' for details
type Snapshot = SlotBounded IX Delegation.Map

-- | Empty (genesis) delegation history
--
-- The delegation history should only be empty if it has never changed.
empty :: DelegationHistory
empty = DelegationHistory Seq.empty

-- | Internal auxiliary
withDelegationHistory :: (StrictSeq Snapshot -> StrictSeq Snapshot)
                      -> DelegationHistory -> DelegationHistory
withDelegationHistory = coerce

-- | Take a snapshot of the delegation state
snapOld :: CC.BlockCount  -- ^ Maximum rollback (@k@)
        -> SlotNo         -- ^ Slot number of the block that changed delegation
        -> Delegation.Map -- ^ Delegation state /before/ it changed
        -> DelegationHistory -> DelegationHistory
snapOld k now old = trim k now . withDelegationHistory append
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
-- We always leave at least one snapshot in the list (see 'DelegationHistory').
trim :: CC.BlockCount  -- ^ Maximum rollback (@k@)
     -> SlotNo         -- ^ Current slot
     -> DelegationHistory -> DelegationHistory
trim k now = withDelegationHistory go
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

find :: WithOrigin SlotNo -> DelegationHistory -> Maybe Snapshot
find slot = Foldable.find (`SB.contains` slot') . toSequence
  where
    slot' :: SlotNo
    slot' = fromWithOrigin genesisSlotNo slot

{-------------------------------------------------------------------------------
  Serialisation

  We translate to @PBftLedgerView@ so that we can piggy-back on its @Serialise@
  instance.
-------------------------------------------------------------------------------}

toLedgerViews :: DelegationHistory
              -> [SlotBounded IX (PBftLedgerView PBftCardanoCrypto)]
toLedgerViews =
      map (fmap toPBftLedgerView)
    . Foldable.toList
    . toSequence

fromLedgerViews :: [SlotBounded IX (PBftLedgerView PBftCardanoCrypto)]
                -> DelegationHistory
fromLedgerViews =
      DelegationHistory
    . Seq.fromList
    . map (fmap fromPBftLedgerView)

encodeDelegationHistory :: DelegationHistory -> Encoding
encodeDelegationHistory = encode . toLedgerViews

decodeDelegationHistory :: Decoder s DelegationHistory
decodeDelegationHistory = fromLedgerViews <$> decode
