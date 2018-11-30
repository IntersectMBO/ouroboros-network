{-# LANGUAGE ScopedTypeVariables #-}

module Test.Protocol.Channel where

import Test.QuickCheck

import Control.Concurrent.Async
import Control.Monad (forM_)

import Protocol.Channel

-- | The one important channel test: for a pair of channels, they deliver
-- messages reliably and ordered. Concurrency is a given here.
prop_channel_reliable_ordered
  :: forall t .
     ( Eq t, Show t )
  => [t]
  -> [t]
  -> Channel IO t
  -> Channel IO t
  -> Property
prop_channel_reliable_ordered send1 send2 chan1 chan2 = ioProperty $ do
  -- Send all of the 'send1's on 'chan1', 'send2's on 'chan2', then receive
  -- them all on the appropriate channels and check that we got what we
  -- expected.
  -- We do sends and receives in mirrored order in the 2 threads so that we
  -- don't make any assumptions about buffering. This will work even for a
  -- 1-place buffered channel.
  (recv1, recv2) <- concurrently
    (forM_ send1 (send chan1) *> receiveThese (length send2) chan1)
    (receiveThese (length send1) chan2 <* forM_ send2 (send chan2))
  pure $ (recv1 === send2) .&&. (recv2 === send1)

  where

  receiveThese :: Monad m => Int -> Channel m t -> m [t]
  receiveThese n chan
    | n <= 0    = pure []
    | otherwise = do
        it <- recv chan
        case it of
          (Nothing, chan') -> pure []
          (Just t, chan')  -> fmap ((:) t) (receiveThese (n-1) chan')
