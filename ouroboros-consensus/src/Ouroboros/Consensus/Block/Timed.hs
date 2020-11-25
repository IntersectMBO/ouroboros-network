{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE TypeFamilies          #-}
module Ouroboros.Consensus.Block.Timed (
    Timed (..)
  , TimeReceived (..)
  ) where

import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Ouroboros.Network.Block
import           Ouroboros.Network.ReceiveDelay

import           Ouroboros.Consensus.BlockchainTime.WallClock.Types

data TimeReceived =
    ReceivedAt !RelativeTime
  | ReadFromDisk
  deriving (Show, Eq, Generic, NoThunks)

-- | A block or header together with time-related information.
data Timed a = Timed {
      getTimed     :: !a
      -- | The time at which the block or header was forged. I.e., the time
      -- corresponding to the slot.
      --
      -- NOTE: because of hard forks, converting a slot to a time is not so
      -- simple. As the slot length can change with each hard fork
    , timeProduced :: !RelativeTime
    , timeReceived :: !TimeReceived
    }
  deriving (Show, Eq, Functor, Generic, NoThunks)

type instance HeaderHash (Timed a) = HeaderHash a

instance StandardHash a => StandardHash (Timed a)

instance HasHeader a => HasHeader (Timed a) where
  getHeaderFields = castHeaderFields . getHeaderFields . getTimed

instance HasReceiveDelay (Timed a) where
  receiveDelay Timed { timeProduced, timeReceived } =
      case timeReceived of
        ReadFromDisk          -> Nothing
        ReceivedAt receivedAt -> Just $ receivedAt `diffRelTime` timeProduced
