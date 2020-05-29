{-# LANGUAGE RecordWildCards #-}

-- | Shift time (for the benefit of tests)
module Ouroboros.Consensus.HardFork.History.ShiftTime (ShiftTime(..)) where

import           Data.Time

import           Ouroboros.Consensus.BlockchainTime.WallClock.Types

import           Ouroboros.Consensus.HardFork.History.Summary

class ShiftTime a where
  shiftTime :: NominalDiffTime -> a -> a

instance ShiftTime SystemStart where
  shiftTime delta (SystemStart start) = SystemStart $ shiftTime delta start

instance ShiftTime a => ShiftTime [a] where
  shiftTime = map . shiftTime

instance ShiftTime (Summary xs) where
  shiftTime delta (Summary summary) = Summary $ shiftTime delta <$> summary

instance ShiftTime EraSummary where
  shiftTime delta EraSummary{..} = EraSummary{
      eraStart  = shiftTime delta eraStart
    , eraEnd    = shiftTime delta eraEnd
    , eraParams = eraParams
    }

instance ShiftTime EraEnd where
  shiftTime delta (EraEnd bound) = EraEnd (shiftTime delta bound)
  shiftTime _     EraUnbounded   = EraUnbounded

instance ShiftTime Bound where
  shiftTime delta Bound{..} = Bound {
      boundTime  = shiftTime delta boundTime
    , boundSlot  = boundSlot
    , boundEpoch = boundEpoch
    }

instance ShiftTime UTCTime where
  shiftTime = addUTCTime
