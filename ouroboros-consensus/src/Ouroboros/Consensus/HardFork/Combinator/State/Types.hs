{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Ouroboros.Consensus.HardFork.Combinator.State.Types (
    -- * Main types
    Current (..)
  , HardForkState (..)
  , Past (..)
  , sequenceHardForkState
    -- * Supporting types
  , TransitionInfo (..)
  , Translate (..)
  , TranslateForecast (..)
  , TranslateLedgerState (..)
  ) where

import           Prelude

import           Control.Monad.Except
import           Data.SOP.Strict
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Forecast
import           Ouroboros.Consensus.HardFork.History (Bound)
import           Ouroboros.Consensus.Ledger.Basics (DiffMK, EmptyMK,
                     LedgerState, LedgerTables,
                     StowableLedgerTables, TableStuff, SwitchLedgerTables)
import           Ouroboros.Consensus.Ticked

import           Ouroboros.Consensus.HardFork.Combinator.Util.Telescope
                     (Telescope)
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Telescope as Telescope

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
  deriving (Eq, Show, Generic, NoThunks)

-- | Thin wrapper around 'Telescope.sequence'
sequenceHardForkState :: forall m f xs. (All Top xs, Functor m)
                      => HardForkState (m :.: f) xs -> m (HardForkState f xs)
sequenceHardForkState (HardForkState tel) =
      fmap HardForkState
    $ Telescope.sequence
    $ hmap sequenceCurrent tel
  where
    sequenceCurrent :: Current (m :.: f) a -> (m :.: Current f) a
    sequenceCurrent (Current start state) =
      Comp $ Current start <$> unComp state

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
-- In addition to the 'Bound' of the transition, this is also told the
-- 'SlotNo' we're constructing a forecast for. This enables the translation
-- function to take into account any scheduled changes that the final ledger
-- view in the preceding era might have.
newtype TranslateForecast f g x y = TranslateForecast {
      translateForecastWith ::
          forall (wt :: SwitchLedgerTables).
           Bound    -- 'Bound' of the transition (start of the new era)
        -> SlotNo   -- 'SlotNo' we're constructing a forecast for
        -> f x wt EmptyMK
        -> Except OutsideForecastRange (Ticked (g y))
    }

-- | Translate a LedgerState across an era transition.
--
-- As new tables might be populated on an era transition, the return map kind is
-- @ValuesMK@. If no tables are populated, normally this will be filled with
-- @polyEmptyLedgerTables@.
data TranslateLedgerState x y = TranslateLedgerState {
        -- | How to translate a Ledger State during the era transition
        translateLedgerStateWith ::
            forall wt.
            ( TableStuff (LedgerState y) wt
            , StowableLedgerTables (LedgerState y) wt
            )
         => EpochNo
         -> LedgerState x wt EmptyMK
         -> LedgerState y wt DiffMK

        -- | How to translate tables on an era transition.
        --
        -- This is a rather technical subtlety. When performing a ledger state
        -- translation, the provided input ledger state will be initially
        -- populated with a @polyEmptyLedgerTables@. This step is required so
        -- that the operation provided to 'Telescope.extend' is an automorphism.
        --
        -- If we only extend by one era, this function is a no-op, as the input
        -- will be empty ledger states. However, if we extend across multiple
        -- eras, previous eras might populate tables thus creating Values that
        -- now need to be translated to newer eras. This function fills that
        -- hole and allows us to promote tables from one era into tables from
        -- the next era.
      , translateLedgerTablesWith ::
          forall wt.
            TableStuff (LedgerState y) wt
         => LedgerTables (LedgerState x) wt DiffMK
         -> LedgerTables (LedgerState y) wt DiffMK
    }

-- | Knowledge in a particular era of the transition to the next era
data TransitionInfo =
    -- | No transition is yet known for this era
    -- We instead record the ledger tip (which must be in /this/ era)
    --
    -- NOTE: If we are forecasting, this will be set to the slot number of the
    -- (past) ledger state in which the forecast was created. This means that
    -- when we construct an 'EpochInfo' using a 'HardForkLedgerView', the
    -- range of that 'EpochInfo' will extend a safe zone from that /past/
    -- ledger state.
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
  deriving (Show, Generic, NoThunks)
