{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Protocol.Channel
  ( Channel (..)
  , uniformChannel
  , fixedInputChannel
  , FromStream (..)
  , useChannel
  , withChannels
  ) where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Free
import Data.Proxy
import Protocol.Core
import Protocol.Transition

data Channel m t = Channel
  { send :: t -> m (Channel m t)
  , recv :: m (Maybe (t, Channel m t))
  }

uniformChannel :: Functor m => (t -> m ()) -> m (Maybe t) -> Channel m t
uniformChannel send recv = Channel
  { send = \t -> uniformChannel send recv <$ send t
  , recv = (fmap . fmap) (flip (,) (uniformChannel send recv)) recv
  }

fixedInputChannel :: Applicative m => [t] -> Channel m t
fixedInputChannel lst = case lst of
  [] -> Channel
    { send = noopSend []
    , recv = pure Nothing
    }
  it@(tr : trs) -> Channel
    { send = noopSend it
    , recv = pure (Just (tr, fixedInputChannel trs))
    }
  where
  noopSend trs = const (pure (fixedInputChannel trs))

data FromStream tr f m t where
  -- | The stream ended even though more input is expected.
  StreamEnd
    :: (Channel m (SomeTransition tr) -> FreeT f m (FromStream tr f m t))
    -> FromStream tr f m t
  -- | An unexpected transition come in.
  StreamUnexpected :: SomeTransition tr -> FromStream tr f m t
  -- | Done using the stream.
  StreamDone :: t -> FromStream tr f m t

useChannel
  :: forall p tr status from to f m t .
     ( Functor f, Monad m )
  => Channel m (SomeTransition tr)
  -> Peer p tr (status from) to f t
  -> FreeT f m (FromStream tr f m t)
useChannel chan peer = case peer of
  PeerDone t -> pure (StreamDone t)
  PeerHole f -> liftF f >>= useChannel chan
  PeerAwait k -> do
    next <- lift $ recv chan
    case next of
      Nothing -> pure $ StreamEnd (flip useChannel peer)
      Just (some, chan') -> case castTransition (Proxy :: Proxy from) some of
        Unexpected -> pure (StreamUnexpected some)
        Expected tr -> useChannel chan' (k tr)
  PeerYield exch next -> do
    chan' <- lift $ send chan (SomeTransition (exchangeTransition exch))
    useChannel chan' next

withChannels :: (Channel IO t -> Channel IO t -> IO r) -> IO r
withChannels k = do
  left <- newEmptyMVar
  right <- newEmptyMVar
  let leftChan = uniformChannel (putMVar left) (fmap Just (takeMVar right))
      rightChan = uniformChannel (putMVar right) (fmap Just (takeMVar left))
  k leftChan rightChan
