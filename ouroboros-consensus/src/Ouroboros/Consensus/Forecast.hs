{-# LANGUAGE DeriveFunctor #-}

module Ouroboros.Consensus.Forecast (
    Forecast(..)
  , mapForecast
  , trivialForecast
  , constantForecastOf
  , OutsideForecastRange(..)
  ) where

import           Control.Exception (Exception)
import           Control.Monad.Except

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Basics (GetTip, getTipSlot)
import           Ouroboros.Consensus.Ticked

-- | Forecast the effect of time ticking
data Forecast a = Forecast {
      forecastAt  :: WithOrigin SlotNo

      -- Precondition: @At s >= forecastAt@
    , forecastFor :: SlotNo -> Except OutsideForecastRange (Ticked a)
    }

mapForecast :: (Ticked a -> Ticked b) -> Forecast a -> Forecast b
mapForecast f (Forecast at for) = Forecast{
      forecastAt  = at
    , forecastFor = fmap f . for
    }

-- | Trivial forecast of values of type @()@ performed by an instance of
-- 'GetTip'.
--
-- Specialization of 'constantForecast'.
trivialForecast :: GetTip b => b -> Forecast ()
trivialForecast x = constantForecastOf TickedTrivial (getTipSlot x)

-- | Forecast where the values are never changing
--
-- This is primarily useful for tests; the forecast range is infinite, but we
-- do still check the precondition, to catch any bugs.
constantForecastOf :: Ticked a -> WithOrigin SlotNo -> Forecast a
constantForecastOf a at = Forecast {
      forecastAt  = at
    , forecastFor = \for ->
                      if NotOrigin for >= at
                        then return a
                        else error "constantForecastOf: precondition violated"
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
