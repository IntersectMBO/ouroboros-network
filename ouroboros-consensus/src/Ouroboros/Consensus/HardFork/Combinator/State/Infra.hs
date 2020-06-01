{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

-- | 'HardForkState' infrastructure that is independent from @.Basics@
--
-- Separate module to avoid circular module dependencies
module Ouroboros.Consensus.HardFork.Combinator.State.Infra (
    -- * Types
    HardForkState_(..)
  , HardForkState
  , initHardForkState
  , Past(..)
  , Snapshot(..)
  , Current(..)
    -- * GC
  , tickAllPast
    -- * Lifting 'Telescope' operations
  , tip
  , match
  , sequence
  , bihczipWith
  , fromTZ
    -- * Aligning
  , Translate(..)
  , align
    -- * Rewinding
  , retractToSlot
    -- * EpochInfo/Summary
  , TransitionOrTip(..)
  , transitionOrTip
  , reconstructSummary
  ) where

import           Prelude hiding (sequence)

import           Codec.Serialise
import           Data.Functor.Identity
import           Data.Functor.Product
import           Data.SOP.Strict hiding (shape)
import           Data.Word
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)
import           Cardano.Slotting.Slot

import           Ouroboros.Consensus.Config.SecurityParam
import           Ouroboros.Consensus.HardFork.History (Bound (..), EraEnd (..),
                     EraParams (..), EraSummary (..))
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Util.Counting
import           Ouroboros.Consensus.Util.SOP

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import           Ouroboros.Consensus.HardFork.Combinator.Util.DerivingVia
import           Ouroboros.Consensus.HardFork.Combinator.Util.InPairs (InPairs,
                     Requiring (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.InPairs as InPairs
import           Ouroboros.Consensus.HardFork.Combinator.Util.Match (Mismatch)
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Match as Match
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Tails as Tails
import           Ouroboros.Consensus.HardFork.Combinator.Util.Telescope
                     (Extend (..), Retract (..), Telescope (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Telescope as Telescope

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | Generic hard fork state
--
-- This is used both for the consensus state and the ledger state.
newtype HardForkState_ g f xs = HardForkState {
      getHardForkState :: Telescope (Past g) (Current f) xs
    }

-- | Hard for state where both 'Past' and 'Current' use the same functor
--
-- In most cases this is what we need; we only end up with different functors
-- after things like 'match'.
type HardForkState f = HardForkState_ f f

-- | Information about the current era
data Current f blk = Current {
      currentStart :: !Bound
    , currentState :: !(f blk)
    }
  deriving (Generic)

-- | Information about a past era
data Past f blk = Past {
      pastStart    :: !Bound
    , pastEnd      :: !Bound
    , pastSnapshot :: !(Snapshot f blk)
    }
  deriving (Generic)

-- | Past snapshot
--
-- We record for each past era how many blocks have been applied to /any/
-- subsequent era. Here is an example with @k = 3@ with three ledgers
-- @A@, @B@ and @C@, with maximum roll back marked for a few states:
--
-- > Initial ledger   Curr A0
-- >
-- > Apply block      Curr A1                      <--\
-- >                                                  |
-- > Transition       Past 0 A1, Curr B0              |
-- > Apply block      Past 1 A1, Curr B1              |  <--\
-- >                                                  |     |
-- > Apply block      Past 2 A1, Curr B2              |     |
-- >                                                  |     |
-- > Transition       Past 2 A1, Past 0 B2, Curr C0   |     |
-- > Apply block      Past 3 A1, Past 1 B2, Curr C1   /     |  <--\
-- >                                                        |     |
-- > Apply block      Past 4 A1, Past 2 B2, Curr C2         |     |
-- > GC               Past GCd,  Past 2 B2, Curr C2         /     |
-- >                                                              |
-- > Apply block      Past GCd,  Past 3 B2, Curr C3               |
-- >                                                              |
-- > Apply block      Past GCd,  Past 4 B2, Curr C4               |
-- > GC               Past GCd,  Past GCd,  Curr C4               /
--
-- Note that at the point where past states are GCed, we indeed can no longer
-- roll back to the point before the corresponding transitions.
data Snapshot f blk =
    -- | Past snapshot still available
    --
    -- Invariant: the count must be @<= k@ (see diagram above).
    Snapshot !Word64 !(f blk)

    -- | Past consensus state not available anymore
    --
    -- After @k@ blocks have been applied, we are sure that we don't need
    -- the old consensus state anymore and so we don't need to keep it around.
  | NoSnapshot
  deriving (Generic)

getSnapshot :: Snapshot f blk -> Maybe (f blk)
getSnapshot (Snapshot _ st) = Just st
getSnapshot NoSnapshot      = Nothing

{-------------------------------------------------------------------------------
  Initialization
-------------------------------------------------------------------------------}

initHardForkState :: f x -> HardForkState f (x ': xs)
initHardForkState st = HardForkState $ TZ $ Current {
      currentStart = History.initBound
    , currentState = st
    }

{-------------------------------------------------------------------------------
  Lifting functions on @f@ to @Current @f@
-------------------------------------------------------------------------------}

lift :: (f blk -> f' blk) -> Current f blk -> Current f' blk
lift f = runIdentity . liftM (Identity . f)

liftM :: Functor m
      => (f blk -> m (f' blk)) -> Current f blk -> m (Current f' blk)
liftM f (Current start cur) = Current start <$> f cur

{-------------------------------------------------------------------------------
  Lifting functions on @f@ to @Past f@
-------------------------------------------------------------------------------}

liftPast :: (f blk -> f' blk) -> Past f blk -> Past f' blk
liftPast f = runIdentity . liftPastM (Identity . f)

liftPastM :: Applicative m
          => (f blk -> m (f' blk)) -> Past f blk -> m (Past f' blk)
liftPastM f (Past start end snapshot) =
    Past start end <$>
      case snapshot of
        NoSnapshot    -> pure NoSnapshot
        Snapshot n st -> Snapshot n <$> f st

{-------------------------------------------------------------------------------
  SOP class instances

  These are convenient, allowing us to treat the 'HardForkState' just like any
  other SOP type; in particular, they deal with lifting functions to 'Current'.
-------------------------------------------------------------------------------}

type instance Prod    (HardForkState_ g)   = NP
type instance SListIN (HardForkState_ g)   = SListI
type instance AllN    (HardForkState_ g) c = All c

instance HAp (HardForkState_ g) where
  hap np (HardForkState st) = HardForkState $
      hap (map_NP' (Fn . lift . apFn) np) st

instance HSequence (HardForkState_ g) where
  hctraverse' = \p f (HardForkState st) -> HardForkState <$>
                                              hctraverse' p (liftM f) st
  htraverse' = hctraverse' (Proxy @Top)
  hsequence' = htraverse' unComp

{-------------------------------------------------------------------------------
  GC
-------------------------------------------------------------------------------}

tickAllPast :: SListI xs
            => SecurityParam -> HardForkState_ g f xs -> HardForkState_ g f xs
tickAllPast k (HardForkState st) = HardForkState $
    Telescope.bihmap (tickPast k) id st

-- | Tick all past states
--
-- This is used to GC past snapshots when they exceed @k@.
tickPast :: SecurityParam -> Past g blk -> Past g blk
tickPast k past = past { pastSnapshot = tickSnapshot k (pastSnapshot past) }

tickSnapshot :: SecurityParam -> Snapshot f blk -> Snapshot f blk
tickSnapshot (SecurityParam k) = \case
    Snapshot n st | n < k -> Snapshot (n + 1) st
    _                     -> NoSnapshot

{-------------------------------------------------------------------------------
  Lift telescope operations
-------------------------------------------------------------------------------}

tip :: SListI xs => HardForkState_ g f xs -> NS f xs
tip (HardForkState st) = hmap currentState $ Telescope.tip st

match :: SListI xs
      => NS h xs
      -> HardForkState_ g f xs
      -> Either (Mismatch h (Current f) xs) (HardForkState_ g (Product h f) xs)
match ns (HardForkState t) =
    HardForkState . hmap distrib <$> Match.matchTelescope ns t
  where
    distrib :: Product h (Current f) blk -> Current (Product h f) blk
    distrib (Pair x (Current start y)) =
        Current start (Pair x y)

sequence :: forall g f m xs. (SListI xs, Functor m)
         => HardForkState_ g (m :.: f) xs -> m (HardForkState_ g f xs)
sequence = \(HardForkState st) -> HardForkState <$>
    Telescope.sequence (hmap distrib st)
  where
    distrib :: Current (m :.: f) blk -> (m :.: Current f) blk
    distrib (Current start st) = Comp $
        Current start <$> unComp st

bihczipWith :: forall xs h g' g f' f. CanHardFork xs
            => (forall blk. SingleEraBlock blk => h blk -> g blk -> g' blk)
            -> (forall blk. SingleEraBlock blk => h blk -> f blk -> f' blk)
            -> NP h xs -> HardForkState_ g f xs -> HardForkState_ g' f' xs
bihczipWith g f ns (HardForkState st) = HardForkState $
    Telescope.bihczipWith proxySingle (liftPast . g) (lift . f) ns st

fromTZ :: HardForkState_ g f '[blk] -> f blk
fromTZ = currentState . Telescope.fromTZ . getHardForkState

{-------------------------------------------------------------------------------
  Aligning
-------------------------------------------------------------------------------}

newtype Translate f x y = Translate {
      translateWith :: EpochNo -> f x -> f y
    }

align :: forall xs h f f' f''. CanHardFork xs
      => InPairs (Translate f) xs
      -> NP (f' -.-> f -.-> f'') xs
      -> HardForkState_ h f'  xs -- ^ State we are aligning with
      -> HardForkState_ f f   xs -- ^ State we are aligning
      -> HardForkState_ f f'' xs
align fs updTip (HardForkState alignWith) (HardForkState toAlign) =
    HardForkState . unI $
      Telescope.alignExtend
        (InPairs.hmap (\f    -> Require $
                       \past -> Extend  $
                       \cur  -> I       $
                         newCurrent f past cur) fs)
        (hmap (fn_2 . liftUpdTip) updTip)
        alignWith
        toAlign
  where
    liftUpdTip :: (f' -.-> f -.-> f'') blk
               -> Current f' blk -> Current f blk -> Current f'' blk
    liftUpdTip f = lift . apFn . apFn f . currentState

    newCurrent :: Translate f blk blk'
               -> Past g' blk
               -> Current f blk
               -> (Past f blk, Current f blk')
    newCurrent f pastG curF = (
          Past    { pastStart    = currentStart curF
                  , pastEnd      = curEnd
                  , pastSnapshot = Snapshot 0 (currentState curF)
                  }
        , Current { currentStart = curEnd
                  , currentState = translateWith f
                                     (boundEpoch curEnd)
                                     (currentState curF)
                  }
        )
      where
        curEnd :: Bound
        curEnd = pastEnd pastG

{-------------------------------------------------------------------------------
  Rewinding
-------------------------------------------------------------------------------}

-- | Rewind until the specified slot is within the era at the tip
retractToSlot :: forall f xs. SListI xs
              => WithOrigin SlotNo
              -> HardForkState f xs -> Maybe (HardForkState f xs)
retractToSlot slot (HardForkState st) =
    HardForkState <$>
      Telescope.retractIf
        (Tails.hpure retract)
        (hmap (fn . containsSlot) (markFirst True sList))
        st
  where
    markFirst :: Bool -> SList xs' -> NP (K Bool) xs'
    markFirst _ SNil  = Nil
    markFirst b SCons = K b :* markFirst False sList

    containsSlot :: K Bool blk -> Past f blk -> K Bool blk
    containsSlot (K isFirst) Past{..} = K $
        case slot of
          Origin -> isFirst -- Assume 'Origin' in the first era
          At s   -> boundSlot pastStart <= s && s < boundSlot pastEnd

    retract :: Retract Maybe (Past f) (Current f) blk blk'
    retract = Retract $ \past _oldCur ->
        Current (pastStart past) <$> getSnapshot (pastSnapshot past)

{-------------------------------------------------------------------------------
  Summary/EpochInfo
-------------------------------------------------------------------------------}

-- | Property of a particular ledger state: transition to the next era if known,
-- or the tip of the ledger otherwise.
data TransitionOrTip =
    -- | Transition to the next era has been confirmed and is stable
    TransitionAt !(WithOrigin SlotNo) !EpochNo

    -- | Transition to the next era not yet known; we reported ledger tip
  | LedgerTip !(WithOrigin SlotNo)
  deriving (Show)

transitionOrTip :: SingleEraBlock blk
                => WrapPartialLedgerConfig blk
                -> LedgerState blk
                -> TransitionOrTip
transitionOrTip cfg st =
    case singleEraTransition' cfg st of
      Just epoch -> TransitionAt (ledgerTipSlot st) epoch
      Nothing    -> LedgerTip (ledgerTipSlot st)

reconstructSummary :: forall g f xs. CanHardFork xs
                   => History.Shape xs
                   -> NP (f -.-> K TransitionOrTip) xs
                   -- ^ Return the 'EpochNo' of the transition to the next
                   -- era if known, or the 'SlotNo' at the tip otherwise.
                   -> HardForkState_ g f xs
                   -> History.Summary xs
reconstructSummary (History.Shape shape) transition (HardForkState st) =
    History.Summary $ go shape transition st
  where
    go :: All SingleEraBlock xs'
       => Exactly xs' EraParams
       -> NP (f -.-> K TransitionOrTip) xs'
       -> Telescope (Past g) (Current f) xs'
       -> NonEmpty xs' EraSummary
    go (K params :* ss) (_ :* ts) (TS Past{..} t) =
        NonEmptyCons (EraSummary pastStart (EraEnd pastEnd) params) $ go ss ts t
    go (K params :* Nil) _ (TZ Current{..}) =
        -- The current era is the last. We assume it lasts until all eternity.
        NonEmptyOne (EraSummary currentStart EraUnbounded params)
    go (K params :* K nextParams :* _) (t :* _) (TZ Current{..}) =
        case unK $ apFn t currentState of
          TransitionAt _tip epoch ->
            -- We haven't reached the next era yet, but the transition is
            -- already known. The safe zone applies from the start of the
            -- next era.
            let currentEnd = History.mkUpperBound params currentStart epoch
                nextStart  = currentEnd
            in NonEmptyCons EraSummary {
                   eraStart  = currentStart
                 , eraParams = params
                 , eraEnd    = EraEnd currentEnd
                 }
             $ NonEmptyOne EraSummary {
                   eraStart  = nextStart
                 , eraParams = nextParams
                 , eraEnd    = applySafeZone
                                 nextParams
                                 nextStart
                                 (At (boundSlot nextStart))
                 }
          LedgerTip ledgerTip -> NonEmptyOne $
            -- The transition to the /next/ era is not yet known, but it's
            -- possible that the tip was actually in the previous era. If that
            -- is the case, the safe zone of /this/ era extends from the start
            -- of this era.  Otherwise, the safe zone extends from the current
            -- ledger tip.
            EraSummary {
                eraStart  = currentStart
              , eraParams = params
              , eraEnd    = applySafeZone
                              params
                              currentStart
                              (max ledgerTip (At (boundSlot currentStart)))
              }

    go Nil _ t = case t of {}

    -- Apply safe zone from the specified 'SlotNo'
    --
    -- All arguments must be referring to or in the same era.
    applySafeZone :: EraParams -> Bound -> WithOrigin SlotNo -> EraEnd
    applySafeZone params@EraParams{..} start =
          History.mkEraEnd params start
        . History.maxMaybeEpoch (History.safeBeforeEpoch eraSafeZone)
        . History.slotToEpochBound params start
        . History.addSlots (History.safeFromTip eraSafeZone)
        . fromWithOrigin (boundSlot start)

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

deriving instance Eq                 (f blk) => Eq                 (Current f blk)
deriving instance Show               (f blk) => Show               (Current f blk)
deriving instance NoUnexpectedThunks (f blk) => NoUnexpectedThunks (Current f blk)

deriving instance Eq                 (f blk) => Eq                 (Past f blk)
deriving instance Show               (f blk) => Show               (Past f blk)
deriving instance NoUnexpectedThunks (f blk) => NoUnexpectedThunks (Past f blk)

deriving instance Eq                 (f blk) => Eq                 (Snapshot f blk)
deriving instance Show               (f blk) => Show               (Snapshot f blk)
deriving instance NoUnexpectedThunks (f blk) => NoUnexpectedThunks (Snapshot f blk)

deriving via LiftTelescope (Past g) (Current f) xs
         instance ( CanHardFork xs
                  , forall blk. SingleEraBlock blk => Show (f blk)
                  , forall blk. SingleEraBlock blk => Show (g blk)
                  ) => Show (HardForkState_ g f xs)

deriving via LiftTelescope (Past g) (Current f) xs
         instance ( CanHardFork xs
                  , forall blk. SingleEraBlock blk => Eq (f blk)
                  , forall blk. SingleEraBlock blk => Eq (g blk)
                  ) => Eq (HardForkState_ g f xs)

deriving via LiftNamedTelescope "HardForkState" (Past g) (Current f) xs
         instance ( CanHardFork xs
                  , forall blk. SingleEraBlock blk => NoUnexpectedThunks (f blk)
                  , forall blk. SingleEraBlock blk => NoUnexpectedThunks (g blk)
                  ) => NoUnexpectedThunks (HardForkState_ g f xs)

{-------------------------------------------------------------------------------
  Serialisation

  This is primarily useful for tests.
-------------------------------------------------------------------------------}

deriving instance Serialise (f blk) => Serialise (Current  f blk)
deriving instance Serialise (f blk) => Serialise (Past     f blk)
deriving instance Serialise (f blk) => Serialise (Snapshot f blk)
