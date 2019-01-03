{-# OPTIONS_GHC "-fno-warn-name-shadowing" #-}

module Ouroboros.Network.Protocol.Channel.Sim
  ( simStmChannels
  , delayChannel
  ) where

import qualified Data.Sequence as Seq

import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadTimer
import Protocol.Channel


-- | Use the simulation STM (via MonadClass) to get 2 channels, one for each
-- peer. There's no notion of EOF for such a channel, so receiving never gives
-- 'Nothing'.
--
-- The channels are unbounded.
simStmChannels
  :: ( Monad m, MonadSTM m )
  => Tr m (Channel m t, Channel m t)
simStmChannels = do
  varA <- newTVar mempty
  varB <- newTVar mempty
  let send = \it m -> atomically (modifyTVar' it (Seq.|> m))
      recv = \it -> atomically $ do
        msgs <- readTVar it
        case Seq.viewl msgs of
          Seq.EmptyL       -> retry
          msg Seq.:< msgs' -> writeTVar it msgs' >> return (Just msg)
      channelA = uniformChannel (send varA) (recv varB)
      channelB = uniformChannel (send varB) (recv varA)
  pure (channelA, channelB)

-- | Delay a channel on the receiver end.
--
delayChannel :: ( Applicative sm
                , MonadSTM rm
                , MonadTimer rm
                )
             => Duration (Time rm)
             -> Duplex sm rm send recv
             -> Duplex sm rm send recv
delayChannel delay = channelRecvEffect (\_ -> threadDelay delay)
