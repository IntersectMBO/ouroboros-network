module Ouroboros.Consensus.Util.Time (
    -- Conversions
    nominalDelay
  , secondsToNominalDiffTime
  ) where

import           Data.Time (DiffTime, NominalDiffTime)

{-------------------------------------------------------------------------------
  Conversions
-------------------------------------------------------------------------------}

nominalDelay :: NominalDiffTime -> DiffTime
nominalDelay = realToFrac

secondsToNominalDiffTime :: Double -> NominalDiffTime
secondsToNominalDiffTime = realToFrac
