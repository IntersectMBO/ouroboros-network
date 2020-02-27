{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

-- | Delegation history
--
-- Intended for qualified import
module Ouroboros.Consensus.Byron.Ledger.DelegationHistory (
    DelegationHistory
  , empty
  , snapOld
  , find
    -- * Serialisation
  , encodeDelegationHistory
  , decodeDelegationHistory
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding, encodeListLen)
import           Codec.Serialise (decode, encode)
import           Control.Applicative ((<|>))
import           Control.Monad (guard)
import           Data.Coerce
import qualified Data.Foldable as Foldable
import           Data.Sequence.Strict (StrictSeq ((:<|), (:|>), Empty))
import qualified Data.Sequence.Strict as Seq
import           GHC.Generics (Generic)

import           Cardano.Binary (enforceSize)
import           Cardano.Prelude (NoUnexpectedThunks)

import qualified Cardano.Chain.Common as CC
import qualified Cardano.Chain.Delegation as Delegation
import           Cardano.Slotting.SlotBounded (Bounds (..), SlotBounded (..))
import qualified Cardano.Slotting.SlotBounded as SB

import           Ouroboros.Network.Block (SlotNo (..))
import           Ouroboros.Network.Point (WithOrigin (..))

import           Ouroboros.Consensus.Protocol.PBFT
import           Ouroboros.Consensus.Util (firstJust)

import           Ouroboros.Consensus.Byron.Ledger.PBFT
import           Ouroboros.Consensus.Byron.Protocol

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
-- bounded in size. The history will be empty if
--
-- * The delegation state never changed, or
-- * We cannot roll back far enough to require a historical delegation state.
--
-- Since the (inclusive) lower bound of the next snapshot will be set to be
-- equal to the (exclusive) upper bound of the previous, we must remember this
-- previous bound even when the ledger is empty. Near genesis we will set this
-- at @Origin@, which will indeed by the lower bound for the first snapshot.
data DelegationHistory = DelegationHistory {
      historyAnchor    :: !(WithOrigin SlotNo)
    , historySnapshots :: !Snapshots
    }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NoUnexpectedThunks)

-- | Snapshots strictly after genesis
--
-- More recent snapshots are stored at the end of the sequence.
--
-- Invariant: the (exclusive) upper bound of each snapshot must equal the
-- (inclusive) lower bound of the next.
type Snapshots = StrictSeq Snapshot

-- | Historical snapshot of the delegation state
--
-- See 'DelegationHistory' for details
type Snapshot = SlotBounded 'IX Delegation.Map

-- | Empty (genesis) delegation history
empty :: DelegationHistory
empty = DelegationHistory Origin Empty

-- | Take a snapshot of the delegation state
snapOld :: CC.BlockCount  -- ^ Maximum rollback (@k@)
        -> SlotNo         -- ^ Slot number of the block that changed delegation
        -> Delegation.Map -- ^ Delegation state /before/ it changed
        -> DelegationHistory -> DelegationHistory
snapOld k now old = trim k now . go
  where
    go :: DelegationHistory -> DelegationHistory
    go h@DelegationHistory{..} = DelegationHistory {
          historyAnchor    = historyAnchor
        , historySnapshots = historySnapshots
                         :|> SB.bounded (lowerBound h) now old
        }

    -- Compute the (inclusive) lower bound for this snapshot
    --
    -- Must be equal to the (exclusive) upper bound for the preceding snapshot;
    -- if the history is empty, we use its anchor instead.
    lowerBound :: DelegationHistory -> WithOrigin SlotNo
    lowerBound DelegationHistory{..} =
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
-- We always leave at least one snapshot in the list (see 'DelegationHistory').
trim :: CC.BlockCount  -- ^ Maximum rollback (@k@)
     -> SlotNo         -- ^ Current slot
     -> DelegationHistory -> DelegationHistory
trim k now h@DelegationHistory{..} =
    case trimSeq historySnapshots of
      Nothing      -> h
      Just (s, ss) -> DelegationHistory (At (sbUpper s)) ss
  where
    -- Earliest slot we might roll back to
    earliest :: WithOrigin SlotNo
    earliest = if now >= (2 * coerce k)
                 then At $ now - (2 * coerce k)
                 else Origin

    -- Trim the list of snapshots, if we can
    -- Returns the most recent snapshot we dropped
    trimSeq :: Snapshots -> Maybe (Snapshot, Snapshots)
    trimSeq Empty      = Nothing
    trimSeq (s :<| ss) = do guard (not (s `SB.contains` earliest))
                            trimSeq ss <|> Just (s, ss)

find :: WithOrigin SlotNo -> DelegationHistory -> Maybe Delegation.Map
find slot = firstJust (`SB.at` slot) . historySnapshots

{-------------------------------------------------------------------------------
  Serialisation

  We translate to @PBftLedgerView@ so that we can piggy-back on its @Serialise@
  instance.
-------------------------------------------------------------------------------}

toLedgerViews :: Snapshots
              -> [SlotBounded 'IX (PBftLedgerView PBftByronCrypto)]
toLedgerViews = map (fmap toPBftLedgerView) . Foldable.toList

fromLedgerViews :: [SlotBounded 'IX (PBftLedgerView PBftByronCrypto)]
                -> Snapshots
fromLedgerViews = Seq.fromList . map (fmap fromPBftLedgerView)

encodeDelegationHistory :: DelegationHistory -> Encoding
encodeDelegationHistory DelegationHistory{..} = mconcat [
      encodeListLen 2
    , encode historyAnchor
    , encode $ toLedgerViews historySnapshots
    ]

decodeDelegationHistory :: Decoder s DelegationHistory
decodeDelegationHistory = do
    enforceSize "DelegationHistory" 2
    historyAnchor    <- decode
    historySnapshots <- fromLedgerViews <$> decode
    return DelegationHistory{..}
