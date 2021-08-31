{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module LedgerOnDisk.Util.RandomTime where

import Data.Time
import System.Random.Stateful
import Data.Fixed
import Data.Coerce
import Control.Monad.IO.Class
import GHC.Generics

newtype RandomNominalDiffTime = RandomNominalDiffTime { unRandomNominalDiffTime :: NominalDiffTime }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Num)

instance UniformRange RandomNominalDiffTime where
  uniformRM (nominalDiffTimeToSeconds . coerce -> MkFixed l, nominalDiffTimeToSeconds . coerce -> MkFixed u) g =
    coerce . secondsToNominalDiffTime . MkFixed <$> uniformRM (l, u) g

randomIO :: (MonadIO m, Uniform a) => m a
randomIO = getStdRandom $ \stdgen -> runStateGen stdgen uniformM

randomRIO :: (MonadIO m, UniformRange a) => (a, a) -> m a
randomRIO range = getStdRandom $ \stdgen -> runStateGen stdgen $ uniformRM range
