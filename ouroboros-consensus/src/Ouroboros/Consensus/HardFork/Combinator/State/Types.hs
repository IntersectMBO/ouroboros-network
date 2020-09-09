{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Ouroboros.Consensus.HardFork.Combinator.State.Types (
    -- * Main types
    HardForkState(..)
  , Current(..)
  , Past(..)
    -- * Supporting types
  , Translate(..)
  , TranslateForecast(..)
  , TransitionInfo(..)
  ) where

import           Prelude hiding (sequence)

import           Data.SOP.Strict (K)
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HardFork.History (Bound)
import           Ouroboros.Consensus.Ticked

import           Ouroboros.Consensus.HardFork.Combinator.Util.Telescope
                     (Telescope)

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | Generic hard fork state
--
-- This is used both for the consensus state and the ledger state.
newtype HardForkState f xs = HardForkState {
      getHardForkState :: Telescope (K Past) (Current f) xs
    }

-- | Information about the current era
data Current f blk = Current {
      currentStart :: !Bound
    , currentState :: !(f blk)
    }
  deriving (Generic)

-- | Information about a past era
data Past = Past {
      pastStart :: !Bound
    , pastEnd   :: !Bound
    }
  deriving (Eq, Show, Generic, NoUnexpectedThunks)

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
        -> Ticked (f x)
        -> Ticked (f y)
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
  deriving (Show, Generic, NoUnexpectedThunks)
