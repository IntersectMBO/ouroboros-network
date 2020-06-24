{-# LANGUAGE DeriveGeneric #-}

module Ouroboros.Consensus.HardFork.Combinator.State.Types (
    -- * Main types
    HardForkState_(..)
  , HardForkState
  , Current(..)
  , Past(..)
  , Snapshot(..)
    -- * Supporting types
  , Translate(..)
  , TranslateForecast(..)
  , TransitionInfo(..)
  ) where

import           Prelude hiding (sequence)

import           Data.Word
import           GHC.Generics (Generic)

import           Cardano.Slotting.Slot

import           Ouroboros.Consensus.HardFork.History (Bound)

import           Ouroboros.Consensus.HardFork.Combinator.Util.Telescope
                     (Telescope)

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

{-------------------------------------------------------------------------------
  Supporting types
-------------------------------------------------------------------------------}

-- | Translate @f x@ to @f y@ across an era transition
--
-- Typically @f@ will be 'LedgerState' or 'WrapChainDepState'.
newtype Translate f x y = Translate {
      translateWith :: EpochNo -> f x -> f y
    }

-- | Translate (a forecast of) @f x@ to (a forecast of) @f y@
-- across an era transition.
--
-- Typically @f@ will be 'WrapLedgerView'.
--
-- In addition to the 'EpochNo' of the transition, this is also told the
-- 'SlotNo' we're constructing a forecast for. This enables the translation
-- function to take into account any scheduled changes that the final ledger
-- view in the preceding era might have.
newtype TranslateForecast f x y = TranslateForecast {
      translateForecastWith ::
           EpochNo  -- 'EpochNo' of the transition
        -> SlotNo   -- 'SlotNo' we're constructing a forecast for
        -> f x
        -> f y
    }

-- | Knowledge in a particular era of the transition to the next era
data TransitionInfo =
    -- | No transition is yet known for this era
    -- We instead record the ledger tip (which must be in /this/ era)
    TransitionUnknown !(WithOrigin SlotNo)

    -- | Transition to the next era is known to happen at this 'EpochNo'
  | TransitionKnown !EpochNo

    -- | The transition is impossible
    --
    -- This can be due to one of two reasons:
    --
    -- * We are in the final era
    -- * This era has not actually begun yet (we are forecasting). In this case,
    --   we cannot look past the safe zone of this era and hence, by definition,
    --   the transition to the /next/ era cannot happen.
  | TransitionImpossible
  deriving (Show)
