{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Protocol.Channel
  ( Duplex (..)
  , hoistDuplex
  , Channel
  , uniformChannel
  , fixedInputChannel
  , FromStream (..)
  , useChannel
  , useChannelHomogeneous
  , mvarChannels
  , withMVarChannels
  , channelSendEffect
  ) where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (join)
import qualified Control.Monad.Trans.Class as Trans (lift)
import Control.Monad.Trans.Free
import Data.Proxy
import Protocol.Core
import Protocol.Transition

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

data FromStream tr k t where
  -- | The stream ended even though more input is expected.
  StreamEnd
    :: (Channel m (SomeTransition tr) -> k (FromStream tr k t))
    -> FromStream tr k t
  -- | An unexpected transition come in.
  StreamUnexpected :: SomeTransition tr -> FromStream tr k t
  -- | Done using the stream.
  StreamDone :: t -> FromStream tr k t

instance Show t => Show (FromStream tr k t) where
  show term = case term of
    StreamEnd _        -> "StreamEnd"
    StreamUnexpected _ -> "StreamUnexpected"
    StreamDone t       -> "StreamDone (" ++ show t ++ ")"

-- | Supply input and carry output to/from a 'Peer' by way of a 'Channel'.
-- If the stream ends before the 'Peer' finishes, a continuation is given.
useChannel
  :: forall p tr status from to f m t .
     ( Functor f, Monad m )
  => Channel m (SomeTransition tr)
  -> Peer p tr (status from) to f t
  -> FreeT f m (FromStream tr (FreeT f m) t)
useChannel chan peer = case peer of
  PeerDone t -> pure (StreamDone t)
  PeerLift f -> liftF f >>= useChannel chan
  PeerAwait k -> do
    next <- Trans.lift $ recv chan
    case next of
      (Nothing, chan') -> pure $ StreamEnd (flip useChannel peer)
      (Just some, chan') -> case castTransition (Proxy :: Proxy from) some of
        Unexpected -> pure (StreamUnexpected some)
        Expected tr -> useChannel chan' (k tr)
  PeerYield exch next -> do
    chan' <- Trans.lift $ send chan (SomeTransition (exchangeTransition exch))
    useChannel chan' next

-- | 'useChannel' where the 'Channel' and 'Peer' are in the same monad, and
-- stop the 'Peer' when/if the channel ends.
useChannelHomogeneous
  :: forall p tr status from to m t .
     ( Monad m )
  => Channel m (SomeTransition tr)
  -> Peer p tr (status from) to m t
  -> m (FromStream tr m t)
useChannelHomogeneous chan peer =
  fmap fixupFromStream (iterT join (useChannel chan peer))
  where
  fixupFromStream it = case it of
    StreamDone t        -> StreamDone t
    StreamUnexpected tr -> StreamUnexpected tr
    StreamEnd k         -> StreamEnd $ \chan ->
      let it = k chan
      in  fmap fixupFromStream (iterT join it)

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
