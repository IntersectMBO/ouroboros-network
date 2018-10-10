module Protocol.Channel.Sim
  ( simStmChannels
  ) where

import MonadClass
import SimSTM
import Protocol.Channel

-- | Use the simulation STM (via MonadClass) to get 2 channels, one for each
-- peer. There's no notion of closing such a channel, so receiving never gives
-- 'Nothing'.
simStmChannels
  :: ( Monad m, MonadSTM m stm )
  => m (Channel m t, Channel m t)
simStmChannels = do
  (varA, varB) <- atomically ((,) <$> newTVar Nothing <*> newTVar Nothing)
  let send = \it m -> atomically (writeTVar it (Just m))
      recv = \it -> atomically $ do
        mmsg <- readTVar it
        case mmsg of
          Nothing -> retry
          Just msg -> writeTVar it Nothing >> return (Just msg)
      channelA = uniformChannel (send varA) (recv varB)
      channelB = uniformChannel (send varB) (recv varA)
  pure (channelA, channelB)
