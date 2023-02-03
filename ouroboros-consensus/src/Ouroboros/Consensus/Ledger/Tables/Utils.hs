{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Ouroboros.Consensus.Ledger.Tables.Utils (
    applyLedgerTablesDiffs
  , applyLedgerTablesDiffsTicked
  , attachAndApplyDiffsTicked
  , calculateAdditions
  , calculateDifference
  , calculateDifferenceTicked
  , emptyLedgerTables
  , forgetLedgerTables
  , forgetLedgerTablesDiffs
  , forgetLedgerTablesDiffsTicked
  , forgetLedgerTablesValues
  , forgetLedgerTablesValuesTicked
  , noNewTickingDiffs
  , polyEmptyLedgerTables
  , prependLedgerTablesDiffs
  , prependLedgerTablesDiffsFromTicked
  , prependLedgerTablesDiffsRaw
  , prependLedgerTablesDiffsTicked
  , prependLedgerTablesTrackingDiffs
  , rawReapplyTracking
  , reapplyTrackingTicked
    -- * Testing
  , rawApplyDiffs
  , rawCalculateDifference
  , rawForgetValues
  , rawPrependTrackingDiffs
  ) where

import qualified Data.Map.Diff.Strict.Internal as Diff.Internal

import           Ouroboros.Consensus.Ticked

import           Ouroboros.Consensus.Ledger.Tables
import           Ouroboros.Consensus.Storage.LedgerDB.HD.DiffSeq

-- | When applying a block that is not on an era transition, ticking won't
-- generate new values, so this function can be used to wrap the call to the
-- ledger rules that perform the tick.
noNewTickingDiffs :: TickedTableStuff l
                   => l any
                   -> l DiffMK
noNewTickingDiffs l = withLedgerTables l polyEmptyLedgerTables

{-------------------------------------------------------------------------------
  Utils aliases
-------------------------------------------------------------------------------}

emptyLedgerTables :: TableStuff l => LedgerTables l EmptyMK
emptyLedgerTables = polyEmptyLedgerTables

-- | Empty values for every table
polyEmptyLedgerTables :: forall mk l. (TableStuff l, IsMapKind mk) => LedgerTables l mk
polyEmptyLedgerTables = pureLedgerTables emptyMK

-- Forget all

forgetLedgerTables :: TableStuff l => l mk -> l EmptyMK
forgetLedgerTables l = withLedgerTables l emptyLedgerTables

-- Forget values

rawForgetValues :: TrackingMK k v -> DiffMK k v
rawForgetValues (ApplyTrackingMK _vs d) = ApplyDiffMK d

forgetLedgerTablesValues :: TableStuff l => l TrackingMK -> l DiffMK
forgetLedgerTablesValues = mapOverLedgerTables rawForgetValues

forgetLedgerTablesValuesTicked :: TickedTableStuff l => Ticked1 l TrackingMK -> Ticked1 l DiffMK
forgetLedgerTablesValuesTicked = mapOverLedgerTablesTicked rawForgetValues


-- Forget diffs

rawForgetDiffs :: TrackingMK k v -> ValuesMK k v
rawForgetDiffs (ApplyTrackingMK vs _ds) = ApplyValuesMK vs

forgetLedgerTablesDiffs       ::       TableStuff l =>         l TrackingMK ->         l ValuesMK
forgetLedgerTablesDiffsTicked :: TickedTableStuff l => Ticked1 l TrackingMK -> Ticked1 l ValuesMK
forgetLedgerTablesDiffs       = mapOverLedgerTables rawForgetDiffs
forgetLedgerTablesDiffsTicked = mapOverLedgerTablesTicked rawForgetDiffs

-- Prepend diffs

rawPrependDiffs ::
     (Ord k, Eq v)
  => DiffMK k v -- ^ Earlier differences
  -> DiffMK k v -- ^ Later differences
  -> DiffMK k v
rawPrependDiffs (ApplyDiffMK d1) (ApplyDiffMK d2) = ApplyDiffMK (d1 <> d2)

prependLedgerTablesDiffsRaw        ::       TableStuff l => LedgerTables l DiffMK ->         l DiffMK ->         l DiffMK
prependLedgerTablesDiffs           ::       TableStuff l =>              l DiffMK ->         l DiffMK ->         l DiffMK
prependLedgerTablesDiffsFromTicked :: TickedTableStuff l => Ticked1      l DiffMK ->         l DiffMK ->         l DiffMK
prependLedgerTablesDiffsTicked     :: TickedTableStuff l =>              l DiffMK -> Ticked1 l DiffMK -> Ticked1 l DiffMK
prependLedgerTablesDiffsRaw        = flip (zipOverLedgerTables rawPrependDiffs)
prependLedgerTablesDiffs           = prependLedgerTablesDiffsRaw . projectLedgerTables
prependLedgerTablesDiffsFromTicked = prependLedgerTablesDiffsRaw . projectLedgerTablesTicked
prependLedgerTablesDiffsTicked     = flip (zipOverLedgerTablesTicked rawPrependDiffs) . projectLedgerTables

-- Apply diffs

rawApplyDiffs ::
     Ord k
  => ValuesMK k v -- ^ Values to which differences are applied
  -> DiffMK   k v -- ^ Differences to apply
  -> ValuesMK k v
rawApplyDiffs (ApplyValuesMK vals) (ApplyDiffMK diffs) = ApplyValuesMK $ Diff.Internal.unsafeApplyDiff vals diffs

applyLedgerTablesDiffs       ::       TableStuff l => l ValuesMK ->         l DiffMK ->         l ValuesMK
applyLedgerTablesDiffsTicked :: TickedTableStuff l => l ValuesMK -> Ticked1 l DiffMK -> Ticked1 l ValuesMK
applyLedgerTablesDiffs       = flip (zipOverLedgerTables       $ flip rawApplyDiffs) . projectLedgerTables
applyLedgerTablesDiffsTicked = flip (zipOverLedgerTablesTicked $ flip rawApplyDiffs) . projectLedgerTables

-- Calculate differences

rawCalculateDifference ::
     (Ord k, Eq v)
  => ValuesMK   k v
  -> ValuesMK   k v
  -> TrackingMK k v
rawCalculateDifference (ApplyValuesMK before) (ApplyValuesMK after) = ApplyTrackingMK after (diff before after)

calculateAdditions        ::       TableStuff l =>         l ValuesMK ->                               l TrackingMK
calculateDifference       :: TickedTableStuff l => Ticked1 l ValuesMK ->         l ValuesMK ->         l TrackingMK
calculateDifferenceTicked :: TickedTableStuff l => Ticked1 l ValuesMK -> Ticked1 l ValuesMK -> Ticked1 l TrackingMK
calculateAdditions               after = zipOverLedgerTables       (flip rawCalculateDifference) after polyEmptyLedgerTables
calculateDifference       before after = zipOverLedgerTables       (flip rawCalculateDifference) after (projectLedgerTablesTicked before)
calculateDifferenceTicked before after = zipOverLedgerTablesTicked (flip rawCalculateDifference) after (projectLedgerTablesTicked before)

rawAttachAndApplyDiffs ::
     Ord k
  => DiffMK     k v
  -> ValuesMK   k v
  -> TrackingMK k v
rawAttachAndApplyDiffs (ApplyDiffMK d) (ApplyValuesMK v) = ApplyTrackingMK (Diff.Internal.unsafeApplyDiff v d) d

-- | Replace the tables in the first parameter with the tables of the second
-- parameter after applying the differences in the first parameter to them
attachAndApplyDiffsTicked ::
     TickedTableStuff l
  => Ticked1 l DiffMK
  ->         l ValuesMK
  -> Ticked1 l TrackingMK
attachAndApplyDiffsTicked after before =
    zipOverLedgerTablesTicked rawAttachAndApplyDiffs after
  $ projectLedgerTables before

rawPrependTrackingDiffs ::
      (Ord k, Eq v)
   => TrackingMK k v
   -> TrackingMK k v
   -> TrackingMK k v
rawPrependTrackingDiffs (ApplyTrackingMK v d2) (ApplyTrackingMK _v d1) =
  ApplyTrackingMK v (d1 <> d2)

-- | Mappend the differences in the ledger tables. Keep the ledger state of the
-- first one.
prependLedgerTablesTrackingDiffs ::
     TickedTableStuff l
  => Ticked1 l TrackingMK
  -> Ticked1 l TrackingMK
  -> Ticked1 l TrackingMK
prependLedgerTablesTrackingDiffs after before =
    zipOverLedgerTablesTicked rawPrependTrackingDiffs after
  $ projectLedgerTablesTicked before

rawReapplyTracking ::
     Ord k
  => TrackingMK k v
  -> ValuesMK   k v
  -> TrackingMK k v
rawReapplyTracking (ApplyTrackingMK _v d) (ApplyValuesMK v) = ApplyTrackingMK (Diff.Internal.unsafeApplyDiff v d) d

-- | Replace the tables in the first parameter with the tables of the second
-- parameter after applying the differences in the first parameter to them
reapplyTrackingTicked ::
     TickedTableStuff l
  => Ticked1 l TrackingMK
  ->         l ValuesMK
  -> Ticked1 l TrackingMK
reapplyTrackingTicked after before =
    zipOverLedgerTablesTicked rawReapplyTracking after
  $ projectLedgerTables before
