{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Ouroboros.Network.Protocol.Stream.Client
    ( Client (..)
    , StreamElement (..)
    , newClientSTM
    , streamClient
    ) where

import Control.Monad (unless)
import Numeric.Natural (Natural)

import Protocol.Core
import Ouroboros.Network.Protocol.Stream.Type

import Ouroboros.Network.MonadClass.MonadSTM (MonadSTM (..), MonadTBQueue (..), atomically, retry)

-- | Client which accumulates a values delivered by a corresponding server.
--
data Client m a
  = Client
    { cliHandleData :: StreamElement a -> m () -- ^ await for data
    , cliAwait      :: m ()                    -- ^ await for client to be ready
    , cliWindow     :: Natural                 -- ^ size of client's window
    }

-- | Elemenets written to the internal queue.  @'EndOfStream'@ is used to mark
-- the last element written to the queue.
--
data StreamElement a
  = StreamElement a
  | EndOfStream
  deriving (Eq, Ord, Show)

newClient
  :: forall m a.
     MonadTBQueue m
  => Natural
  -> TBQueue m (StreamElement a)
  -> Client m a
newClient window queue = Client
  { cliHandleData = atomically . writeTBQueue queue
  , cliAwait
  , cliWindow     = window
  }
 where
  cliAwait = atomically $ do
    empty <- isEmptyTBQueue queue
    unless empty retry

-- | Smart constructor for @'Client'@.
--
newClientSTM
  :: forall m a.
     MonadTBQueue m
  => Natural
  -> Tr m (Client m a, TBQueue m (StreamElement a))
newClientSTM window = do
  queue <- newTBQueue window
  return (newClient window queue, queue)

-- | Client side of the stream protocol.
--
streamClient
  :: forall m rng a. MonadTBQueue m
  => Client m a
  -> rng
  -> Peer StreamProtocol (StreamMessage rng a)
      ('Yielding 'StIdle) ('Finished 'StDone)
      m ()
streamClient client r = over (MsgRequest r (cliWindow client)) go
 where
  go :: Peer StreamProtocol (StreamMessage rng a)
          ('Awaiting 'StBusy) ('Finished 'StDone)
          m ()
  go = await $ \resp ->
    case resp of
      -- wait for the client
      MsgRenewWindow -> lift $ do
        cliAwait client
        pure $ over MsgUpdateWindow go
      -- handle data and recurse
      MsgData a      -> lift (cliHandleData client (StreamElement a) >> pure go)
      -- we are done
      MsgStreamEnd   -> lift (cliHandleData client EndOfStream >> pure (done ()))
