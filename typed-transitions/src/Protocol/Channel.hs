{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Protocol.Channel
  ( Duplex (..)
  , hoistDuplex
  , Channel
  , uniformChannel
  , fixedInputChannel
  , mvarChannels
  , withMVarChannels
  , channelSendEffect
  ) where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)

-- | Abstract duplex ordered channel.
-- 'send' and 'recv' produce a new 'Channel m t' so that it's possible to
-- give pure channels without resorting to a state monad.
-- For common effectful channels this is superfluous. 'uniformChannel' can
-- take care of the details.
--
-- A 'Duplex' is morally an additive conjunction (linear logic):
-- you cannot use _both_ 'send' and 'recv'. If you 'send', you can 'recv' from
-- the 'Duplex' that it returns, not the original 'Duplex'.
data Duplex sm rm send recv = Duplex
  { send :: send -> sm (Duplex sm rm send recv)
    -- | If the first component is Nothing then recv on the second component
    -- also gives Nothing. This means there's no more input, but sending
    -- is still possible.
  , recv :: rm (Maybe recv, Duplex sm rm send recv)
  }

hoistDuplex
  :: ( Functor sn, Functor rn )
  => (forall x . sm x -> sn x)
  -> (forall x . rm x -> rn x)
  -> Duplex sm rm send recv
  -> Duplex sn rn send recv
hoistDuplex snat rnat duplex = Duplex
  { send = fmap (hoistDuplex snat rnat) . snat . send duplex
  , recv = (fmap . fmap) (hoistDuplex snat rnat) (rnat (recv duplex))
  }

type Channel m t = Duplex m m t t

uniformChannel :: Functor m => (t -> m ()) -> m (Maybe t) -> Channel m t
uniformChannel send recv = Duplex
  { send = \t -> uniformChannel send recv <$ send t
  , recv = fmap (flip (,) (uniformChannel send recv)) recv
  }

fixedInputChannel :: Applicative m => [t] -> Channel m t
fixedInputChannel lst = case lst of
  [] -> Duplex
    { send = noopSend []
    , recv = pure (Nothing, fixedInputChannel [])
    }
  it@(tr : trs) -> Duplex
    { send = noopSend it
    , recv = pure (Just tr, fixedInputChannel trs)
    }
  where
  noopSend trs = const (pure (fixedInputChannel trs))

mvarChannels :: IO (Channel IO t, Channel IO t)
mvarChannels = do
  left <- newEmptyMVar
  right <- newEmptyMVar
  let leftChan  = uniformChannel (putMVar left)  (fmap Just (takeMVar right))
      rightChan = uniformChannel (putMVar right) (fmap Just (takeMVar left))
  pure (leftChan, rightChan)

withMVarChannels :: (Channel IO t -> Channel IO t -> IO r) -> IO r
withMVarChannels k = do
  (left, right) <- mvarChannels
  k left right

-- | Do some effect before sending.
channelSendEffect :: ( Applicative m ) => (t -> m ()) -> Channel m t -> Channel m t
channelSendEffect onSend chan = chan
  { send = \t -> onSend t *> (channelSendEffect onSend <$> send chan t)
  , recv = (fmap . fmap) (channelSendEffect onSend) (recv chan)
  }
