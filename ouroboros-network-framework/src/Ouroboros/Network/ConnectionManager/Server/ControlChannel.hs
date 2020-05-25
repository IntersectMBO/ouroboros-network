{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Intended to be imported qualified.
--
module Ouroboros.Network.ConnectionManager.Server.ControlChannel
  ( ControlMessage (..)
  , ControlChannel (..)
  , ServerControlChannel
  , newControlChannel
  , newOutboundConnection

  -- * Internals
  , peekAlt
  ) where

import           Control.Applicative (Alternative (..))
import           Control.Exception (SomeException)
import           Control.Monad.Class.MonadSTM.Strict

import           Data.ByteString.Lazy (ByteString)
import           Data.Sequence.Strict (StrictSeq (..), (|>), (><))
import qualified Data.Sequence.Strict as Seq

import           Network.Mux (Mux)
import           Network.Mux.Types (MuxMode)

import           Ouroboros.Network.Mux (MiniProtocol, MiniProtocolNum)
import           Ouroboros.Network.ConnectionId
import           Ouroboros.Network.ConnectionManager.ConnectionHandler


-- | We run a monitoring thread, which listens on these events.
--
data ControlMessage (muxMode :: MuxMode) peerAddr versionNumber m a b

    -- | After accepting a connection and awaiting for 'MuxPromise' we either
    -- can start all mini-protocols or handle errors.
    = NewInboundConnection
      !(MuxPromise muxMode peerAddr versionNumber ByteString m a b)

    -- | After creating an outbound connection, we need to on-demand start
    -- responder protocols.
    --
    | NewOutboundConnection
      !(MuxPromise muxMode peerAddr versionNumber ByteString m a b)

    -- | A mini-protocol thrown an exception; we will either bring whole
    -- connection down or the node (if this happend to be a fatal error).
    --
    | MiniProtocolException
      !(Mux muxMode m)
      !(ConnectionId peerAddr)
      !MiniProtocolNum
      !SomeException  

    -- | Event raised after a successful completion of a mini-protocol.
    --
    | MiniProtocolCompleted
       !(Mux muxMode m)
       !(ConnectionId peerAddr)
       !(MiniProtocol muxMode ByteString m a b)


-- | Server control channel.  It allows to pass 'STM' transactions which will
-- resolve to 'ControlMessages'.   Server's monitoring thread is the consumer
-- of this messages; there are two produceres: accept loop and connection
-- handler for outbound connections.
--
data ControlChannel m controlMessage =
  ControlChannel {
    -- | Read a single 'ControlMessage' instructrion from the channel.
    --
    readControlMessage :: m controlMessage,

    -- | Write a 'ControlMessage' to the channel.
    --
    writeControlMessage :: STM m controlMessage -> m ()
  }


type ServerControlChannel m muxMode peerAddr versionNumber a b =
    ControlChannel m (ControlMessage muxMode peerAddr versionNumber m a b)


newControlChannel :: forall m controlMessage.
                     MonadSTM m
                  => m (ControlChannel m controlMessage)
newControlChannel = do
    channel <- newTVarM Seq.Empty
    pure $ ControlChannel {
        readControlMessage  = readControlMessage channel,
        writeControlMessage = writeControlMessage channel
      }
  where
    readControlMessage
      :: StrictTVar m
           (StrictSeq
             (STM m controlMessage))
      -> m controlMessage
    readControlMessage channel = atomically $ do
      (controlMessage, rest) <- readTVar channel >>= peekAlt
      writeTVar channel rest
      pure controlMessage

    writeControlMessage
      :: StrictTVar m
           (StrictSeq
             (STM m controlMessage))
      -> STM m controlMessage
      -> m ()
    writeControlMessage channel controlMessage = atomically $
      modifyTVar channel (|> controlMessage)


newOutboundConnection
    :: Applicative (STM m)
    => ControlChannel m (ControlMessage muxMode peerAddr versionNumber m a b)
    -> MuxPromise muxMode peerAddr versionNumber ByteString m a b
    -> m ()
newOutboundConnection serverControlChannel =
    writeControlMessage serverControlChannel . pure . NewOutboundConnection


--
-- Internals
--

-- | 'peekAlt' finds first non 'empty' element and returns it together with the
-- sequence of all the other ones (preserving their original order).  Only the
-- returned non-empty element is dropped from the sequence.  It is expressed
-- using 'Alternative' applicative functor, instead of `STM m` for
-- testing purposes.
--
peekAlt :: Alternative m
        => StrictSeq (m a)
        -> m (a, StrictSeq (m a))
peekAlt = go Seq.Empty
  where
    -- in the cons case we either can resolve 'stm', in which case we
    -- return the value together with list of all other transactions, or
    -- (`<|>`) we push it on the `acc` and recrurse.
    go !acc (stm :<| stms) =
      ((\a -> (a, acc >< stms)) <$> stm)
      <|>
      go (acc |> stm) stms
    -- in the 'Empty' case, we just need to 'retry' the trasaction (hence we
    -- use 'empty').
    go _acc Seq.Empty = empty
