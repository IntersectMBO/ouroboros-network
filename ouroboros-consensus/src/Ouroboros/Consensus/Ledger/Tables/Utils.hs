{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE Rank2Types          #-}
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
  , applyLedgerTablesDiffsFromTicked
  , applyLedgerTablesDiffsTicked
  , attachAndApplyDiffsTicked
  , attachAndApplyDiffsTickedToTables
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
  , mapOverLedgerTables
  , mapOverLedgerTablesTicked
  , rawApplyDiffs
  , rawCalculateDifference
  , rawForgetValues
  , rawPrependTrackingDiffs
  , restrictValues
  , zipOverLedgerTables
  , zipOverLedgerTablesTicked
  ) where

import           Data.Map.Diff.Strict
import           Data.Map.Diff.Strict.Internal

import           Ouroboros.Consensus.Ticked

import qualified Data.Map.Strict as Map
import           Ouroboros.Consensus.Ledger.Tables

{-------------------------------------------------------------------------------
  Util combinators
-------------------------------------------------------------------------------}

overLedgerTables ::
     (HasLedgerTables l, IsMapKind mk1, IsMapKind mk2)
  => (LedgerTables l mk1 -> LedgerTables l mk2)
  -> l mk1
  -> l mk2
overLedgerTables f l = withLedgerTables l $ f $ projectLedgerTables l

mapOverLedgerTables ::
     (HasLedgerTables l, IsMapKind mk1, IsMapKind mk2)
  => (forall k v.
          (Ord k, Eq v)
       => mk1 k v
       -> mk2 k v
     )
  -> l mk1
  -> l mk2
mapOverLedgerTables f = overLedgerTables $ mapLedgerTables f

zipOverLedgerTables ::
     (HasLedgerTables l, IsMapKind mk1, IsMapKind mk3)
  => (forall k v.
          (Ord k, Eq v)
       => mk1 k v
       -> mk2 k v
       -> mk3 k v
     )
  ->              l mk1
  -> LedgerTables l mk2
  ->              l mk3
zipOverLedgerTables f l tables2 =
    overLedgerTables
      (\tables1 -> zipLedgerTables f tables1 tables2)
      l

overLedgerTablesTicked ::
     (HasTickedLedgerTables l, IsMapKind mk1, IsMapKind mk2)
  => (LedgerTables l mk1 -> LedgerTables l mk2)
  -> Ticked1 l mk1
  -> Ticked1 l mk2
overLedgerTablesTicked f l =
    withLedgerTablesTicked l $ f $ projectLedgerTablesTicked l

mapOverLedgerTablesTicked ::
     (HasTickedLedgerTables l, IsMapKind mk1, IsMapKind mk2)
  => (forall k v.
         (Ord k, Eq v)
      => mk1 k v
      -> mk2 k v
     )
  -> Ticked1 l mk1
  -> Ticked1 l mk2
mapOverLedgerTablesTicked f = overLedgerTablesTicked $ mapLedgerTables f

zipOverLedgerTablesTicked ::
     (HasTickedLedgerTables l, IsMapKind mk1, IsMapKind mk3)
  => (forall k v.
         (Ord k, Eq v)
      => mk1 k v
      -> mk2 k v
      -> mk3 k v
     )
  -> Ticked1      l mk1
  -> LedgerTables l mk2
  -> Ticked1      l mk3
zipOverLedgerTablesTicked f l tables2 =
    overLedgerTablesTicked
      (\tables1 -> zipLedgerTables f tables1 tables2)
      l

{-------------------------------------------------------------------------------
  Utils aliases
-------------------------------------------------------------------------------}

-- | When applying a block that is not on an era transition, ticking won't
-- generate new values, so this function can be used to wrap the call to the
-- ledger rules that perform the tick.
noNewTickingDiffs :: HasTickedLedgerTables l
                   => l any
                   -> l DiffMK
noNewTickingDiffs l = withLedgerTables l emptyLedgerTables

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

applyLedgerTablesDiffs           ::       HasLedgerTables l =>         l ValuesMK ->         l DiffMK ->         l ValuesMK
applyLedgerTablesDiffsTicked     :: HasTickedLedgerTables l =>         l ValuesMK -> Ticked1 l DiffMK -> Ticked1 l ValuesMK
applyLedgerTablesDiffsFromTicked :: HasTickedLedgerTables l => Ticked1 l ValuesMK ->         l DiffMK ->         l ValuesMK
applyLedgerTablesDiffs           = flip (zipOverLedgerTables       $ flip rawApplyDiffs) . projectLedgerTables
applyLedgerTablesDiffsTicked     = flip (zipOverLedgerTablesTicked $ flip rawApplyDiffs) . projectLedgerTables
applyLedgerTablesDiffsFromTicked = flip (zipOverLedgerTables       $ flip rawApplyDiffs) . projectLedgerTablesTicked

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
attachAndApplyDiffsTickedToTables ::
     HasTickedLedgerTables l
  => Ticked1 l DiffMK
  -> LedgerTables l ValuesMK
  -> Ticked1 l TrackingMK
attachAndApplyDiffsTickedToTables =
    zipOverLedgerTablesTicked rawAttachAndApplyDiffs

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

rawRestrictValues ::
     Ord k
  => ValuesMK k v
  -> KeysMK k v
  -> ValuesMK k v
rawRestrictValues (ValuesMK v) (KeysMK k) = ValuesMK $ v `Map.restrictKeys` k

restrictValues ::
     HasLedgerTables l
  => l ValuesMK
  -> LedgerTables l KeysMK
  -> LedgerTables l ValuesMK
restrictValues st = zipLedgerTables rawRestrictValues (projectLedgerTables st)
