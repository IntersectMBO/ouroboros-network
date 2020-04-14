{-# LANGUAGE DeriveFunctor #-}

module Ouroboros.Consensus.Forecast (
    Forecast(..)
  , trivialForecast
  , constantForecastOf
  , OutsideForecastRange(..)
  ) where

import           Control.Exception (Exception)
import           Control.Monad.Except

import           Cardano.Slotting.Slot hiding (at)

data Forecast a = Forecast {
      forecastAt  :: WithOrigin SlotNo


      -- Precondition: @At s >= forecastAt@
    , forecastFor :: SlotNo -> Except OutsideForecastRange a
    }
  deriving (Functor)

-- | Trivial forecast of values of type @()@
--
-- Specialization of 'constantForecast'.
trivialForecast :: WithOrigin SlotNo -> Forecast ()
trivialForecast = constantForecastOf ()

-- | Forecast where the values are never changing
--
-- This is primarily useful for tests; the forecast range is infinite, but we
-- do still check the precondition, to catch any bugs.
constantForecastOf :: a -> WithOrigin SlotNo -> Forecast a
constantForecastOf a at = Forecast {
      forecastAt  = at
    , forecastFor = \for ->
                      if At for >= at
                        then return a
                        else error "trivialForecast: precondition violated"
    }

data OutsideForecastRange =
    OutsideForecastRange {
        -- | The slot for which the forecast was obtained
        outsideForecastAt     :: !(WithOrigin SlotNo)

        -- | Exclusive upper bound on the range of the forecast
      , outsideForecastMaxFor :: !SlotNo

        -- | The slot for which we requested a value
      , outsideForecastFor    :: !SlotNo
      }
  deriving (Show, Eq)

instance Exception OutsideForecastRange
