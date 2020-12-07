{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Util.Time (
    -- Conversions
    nominalDelay
  , secondsToNominalDiffTime
  ) where

import           Data.Time (DiffTime, NominalDiffTime)
import qualified Data.Time as Time

-- adapted from time-1.11.1.1
instance Read NominalDiffTime where
  readsPrec _ r =
      [ (Time.secondsToNominalDiffTime m, t)
      | (m,'s':t) <- readsPrec 11 r
      ]

{-------------------------------------------------------------------------------
  Conversions
-------------------------------------------------------------------------------}

nominalDelay :: NominalDiffTime -> DiffTime
nominalDelay = realToFrac

secondsToNominalDiffTime :: Double -> NominalDiffTime
secondsToNominalDiffTime = realToFrac
