{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | A collection of useful combinators to shorten the code in other places.
--
-- TODO: #4394 provide better ergonomics. This whole module provides ways to
-- combine tables of two ledger states to produce another one. It is written
-- very much ad-hoc and we should probably think of some way to make this more
-- ergonomic. In particular for functions that take two ledger states, it is
-- unclear if it will keep the in-memory part of the first or the second one.
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

import           Data.Map.Diff.Strict
import           Data.Map.Diff.Strict.Internal
import           Ouroboros.Consensus.Ledger.Tables
import           Ouroboros.Consensus.Ticked

-- | When applying a block that is not on an era transition, ticking won't
-- generate new values, so this function can be used to wrap the call to the
-- ledger rules that perform the tick.
noNewTickingDiffs :: HasTickedLedgerTables l
                   => l any
                   -> l DiffMK
noNewTickingDiffs l = withLedgerTables l emptyLedgerTables

{-------------------------------------------------------------------------------
  Utils aliases
-------------------------------------------------------------------------------}

-- | Empty values for every table
emptyLedgerTables :: forall mk l. (HasLedgerTables l, IsMapKind mk) => LedgerTables l mk
emptyLedgerTables = pureLedgerTables emptyMK

-- Forget all

forgetLedgerTables :: HasLedgerTables l => l mk -> l EmptyMK
forgetLedgerTables l = withLedgerTables l emptyLedgerTables

-- Forget values

rawForgetValues :: TrackingMK k v -> DiffMK k v
rawForgetValues (TrackingMK _vs d) = DiffMK d

forgetLedgerTablesValues :: HasLedgerTables l => l TrackingMK -> l DiffMK
forgetLedgerTablesValues = mapOverLedgerTables rawForgetValues

forgetLedgerTablesValuesTicked :: HasTickedLedgerTables l => Ticked1 l TrackingMK -> Ticked1 l DiffMK
forgetLedgerTablesValuesTicked = mapOverLedgerTablesTicked rawForgetValues

-- Forget diffs

rawForgetDiffs :: TrackingMK k v -> ValuesMK k v
rawForgetDiffs (TrackingMK vs _ds) = ValuesMK vs

forgetLedgerTablesDiffs       ::       HasLedgerTables l =>         l TrackingMK ->         l ValuesMK
forgetLedgerTablesDiffsTicked :: HasTickedLedgerTables l => Ticked1 l TrackingMK -> Ticked1 l ValuesMK
forgetLedgerTablesDiffs       = mapOverLedgerTables rawForgetDiffs
forgetLedgerTablesDiffsTicked = mapOverLedgerTablesTicked rawForgetDiffs

-- Prepend diffs

rawPrependDiffs ::
     (Ord k, Eq v)
  => DiffMK k v -- ^ Earlier differences
  -> DiffMK k v -- ^ Later differences
  -> DiffMK k v
rawPrependDiffs (DiffMK d1) (DiffMK d2) = DiffMK (d1 <> d2)

prependLedgerTablesDiffsRaw        ::       HasLedgerTables l => LedgerTables l DiffMK ->         l DiffMK ->         l DiffMK
prependLedgerTablesDiffs           ::       HasLedgerTables l =>              l DiffMK ->         l DiffMK ->         l DiffMK
prependLedgerTablesDiffsFromTicked :: HasTickedLedgerTables l => Ticked1      l DiffMK ->         l DiffMK ->         l DiffMK
prependLedgerTablesDiffsTicked     :: HasTickedLedgerTables l =>              l DiffMK -> Ticked1 l DiffMK -> Ticked1 l DiffMK
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
rawApplyDiffs (ValuesMK vals) (DiffMK diffs) = ValuesMK (unsafeApplyDiff vals diffs)

applyLedgerTablesDiffs       ::       HasLedgerTables l => l ValuesMK ->         l DiffMK ->         l ValuesMK
applyLedgerTablesDiffsTicked :: HasTickedLedgerTables l => l ValuesMK -> Ticked1 l DiffMK -> Ticked1 l ValuesMK
applyLedgerTablesDiffs       = flip (zipOverLedgerTables       $ flip rawApplyDiffs) . projectLedgerTables
applyLedgerTablesDiffsTicked = flip (zipOverLedgerTablesTicked $ flip rawApplyDiffs) . projectLedgerTables

-- Calculate differences

rawCalculateDifference ::
     (Ord k, Eq v)
  => ValuesMK   k v
  -> ValuesMK   k v
  -> TrackingMK k v
rawCalculateDifference (ValuesMK before) (ValuesMK after) = TrackingMK after (diff before after)

calculateAdditions        ::       HasLedgerTables l =>         l ValuesMK ->                               l TrackingMK
calculateDifference       :: HasTickedLedgerTables l => Ticked1 l ValuesMK ->         l ValuesMK ->         l TrackingMK
calculateDifferenceTicked :: HasTickedLedgerTables l => Ticked1 l ValuesMK -> Ticked1 l ValuesMK -> Ticked1 l TrackingMK
calculateAdditions               after = zipOverLedgerTables       (flip rawCalculateDifference) after emptyLedgerTables
calculateDifference       before after = zipOverLedgerTables       (flip rawCalculateDifference) after (projectLedgerTablesTicked before)
calculateDifferenceTicked before after = zipOverLedgerTablesTicked (flip rawCalculateDifference) after (projectLedgerTablesTicked before)

rawAttachAndApplyDiffs ::
     Ord k
  => DiffMK     k v
  -> ValuesMK   k v
  -> TrackingMK k v
rawAttachAndApplyDiffs (DiffMK d) (ValuesMK v) = TrackingMK (unsafeApplyDiff v d) d

-- | Replace the tables in the first parameter with the tables of the second
-- parameter after applying the differences in the first parameter to them
attachAndApplyDiffsTicked ::
     HasTickedLedgerTables l
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
rawPrependTrackingDiffs (TrackingMK v d2) (TrackingMK _v d1) =
  TrackingMK v (d1 <> d2)

-- | Mappend the differences in the ledger tables. Keep the ledger state of the
-- first one.
prependLedgerTablesTrackingDiffs ::
     HasTickedLedgerTables l
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
rawReapplyTracking (TrackingMK _v d) (ValuesMK v) = TrackingMK (unsafeApplyDiff v d) d

-- | Replace the tables in the first parameter with the tables of the second
-- parameter after applying the differences in the first parameter to them
reapplyTrackingTicked ::
     HasTickedLedgerTables l
  => Ticked1 l TrackingMK
  ->         l ValuesMK
  -> Ticked1 l TrackingMK
reapplyTrackingTicked after before =
    zipOverLedgerTablesTicked rawReapplyTracking after
  $ projectLedgerTables before
