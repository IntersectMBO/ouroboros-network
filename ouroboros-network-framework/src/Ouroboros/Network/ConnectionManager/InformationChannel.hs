{-# LANGUAGE RankNTypes #-}
module Ouroboros.Network.ConnectionManager.InformationChannel where

import           Control.Concurrent.Class.MonadSTM.Strict

import           Data.Functor (($>))
import           GHC.Natural (Natural)

-- | Information channel.
--
data InformationChannel a m =
  InformationChannel {
    -- | Read a single value from the channel.
    --
    readMessage  :: STM m a,

    -- | Write a value to the channel.
    --
    writeMessage :: a -> STM m ()
  }


newInformationChannel :: forall a m.
                        MonadLabelledSTM m
                      => m (InformationChannel a m)
newInformationChannel = do
    channel <-
      atomically $
        newTBQueue cc_QUEUE_BOUND
        >>= \q -> labelTBQueue q "server-cc" $> q
    pure $ InformationChannel {
        readMessage  = readTBQueue channel,
        writeMessage = writeTBQueue channel
      }

-- | The 'InformationChannel's 'TBQueue' depth.
--
cc_QUEUE_BOUND :: Natural
cc_QUEUE_BOUND = 10
