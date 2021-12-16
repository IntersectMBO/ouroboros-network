{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE EmptyCase           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeInType          #-}

-- | Actions for running 'Peer's with a 'Driver'
--
module Network.TypedProtocol.Driver
  ( -- * Introduction
    -- $intro
    -- * Driver interface
    Driver (..)
  , SomeMessage (..)
    -- * Normal peers
  , runPeerWithDriver
    -- * Pipelined peers
  , runPipelinedPeerWithDriver
  ) where

import           Data.Void (Void)

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Pipelined

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSTM


-- $intro
--
-- A 'Peer' is a particular implementation of an agent that engages in a
-- typed protocol. To actualy run one we need a source and sink for the typed
-- protocol messages. These are provided by a 'Channel' and a 'Codec'. The
-- 'Channel' represents one end of an untyped duplex message transport, and
-- the 'Codec' handles conversion between the typed protocol messages and
-- the untyped channel.
--
-- So given the 'Peer' and a compatible 'Codec' and 'Channel' we can run the
-- peer in some appropriate monad. The peer and codec have to agree on
-- the same protocol and role in that protocol. The codec and channel have to
-- agree on the same untyped medium, e.g. text or bytes. All three have to
-- agree on the same monad in which they will run.
--
-- This module provides drivers for normal and pipelined peers. There is
-- very little policy involved here so typically it should be possible to
-- use these drivers, and customise things by adjusting the peer, or codec
-- or channel.
--
-- It is of course possible to write custom drivers and the code for these ones
-- may provide a useful starting point. The 'runDecoder' function may be a
-- helpful utility for use in custom drives.
--


--
-- Driver interface
--

data Driver ps dstate m =
        Driver {
          sendMessage :: forall (pr :: PeerRole) (st :: ps) (st' :: ps).
                         PeerHasAgency pr st
                      -> Message ps st st'
                      -> m ()

        , recvMessage :: forall (pr :: PeerRole) (st :: ps).
                         PeerHasAgency pr st
                      -> dstate
                      -> m (SomeMessage st, dstate)

        , startDState :: dstate
        }

-- | When decoding a 'Message' we only know the expected \"from\" state. We
-- cannot know the \"to\" state as this depends on the message we decode. To
-- resolve this we use the 'SomeMessage' wrapper which uses an existential
-- type to hide the \"to"\ state.
--
data SomeMessage (st :: ps) where
     SomeMessage :: Message ps st st' -> SomeMessage st


--
-- Running normal non-pipelined peers
--

-- | Run a peer with the given driver.
--
-- This runs the peer to completion (if the protocol allows for termination).
--
runPeerWithDriver
  :: forall ps (st :: ps) pr dstate m a.
     Monad m
  => Driver ps dstate m
  -> Peer ps pr st m a
  -> dstate
  -> m (a, dstate)
runPeerWithDriver Driver{sendMessage, recvMessage} =
    flip go
  where
    go :: forall st'.
          dstate
       -> Peer ps pr st' m a
       -> m (a, dstate)
    go dstate (Effect k) = k >>= go dstate
    go dstate (Done _ x) = return (x, dstate)

    go dstate (Yield stok msg k) = do
      sendMessage stok msg
      go dstate k

    go dstate (Await stok k) = do
      (SomeMessage msg, dstate') <- recvMessage stok dstate
      go dstate' (k msg)

    -- Note that we do not complain about trailing data in any case, neither
    -- the 'Await' nor 'Done' cases.
    --
    -- We want to be able to use a non-pipelined peer in communication with
    -- a pipelined peer, and in that case the non-pipelined peer will in
    -- general see trailing data after an 'Await' which is the next incoming
    -- message.
    --
    -- Likewise for 'Done', we want to allow for one protocols to be run after
    -- another on the same channel. It would be legal for the opening message
    -- of the next protocol arrives in the same data chunk as the final
    -- message of the previous protocol.


--
-- Running pipelined peers
--

-- | Run a pipelined peer with the given driver.
--
-- This runs the peer to completion (if the protocol allows for termination).
--
-- Unlike normal peers, running pipelined peers rely on concurrency, hence the
-- 'MonadAsync' constraint.
--
runPipelinedPeerWithDriver
  :: forall ps (st :: ps) pr dstate m a.
     MonadAsync m
  => Driver ps dstate m
  -> PeerPipelined ps pr st m a
  -> dstate
  -> m (a, dstate)
runPipelinedPeerWithDriver driver (PeerPipelined peer) dstate0 = do
    receiveQueue <- atomically newTQueue
    collectQueue <- atomically newTQueue
    a <- runPipelinedPeerReceiverQueue receiveQueue collectQueue driver
           `withAsyncLoop`
         runPipelinedPeerSender        receiveQueue collectQueue driver
                                       peer dstate0
    return a

  where
    withAsyncLoop :: m Void -> m x -> m x
    withAsyncLoop left right = do
      -- race will throw if either of the threads throw
      res <- race left right
      case res of
        Left v  -> case v of {}
        Right a -> return a

data ReceiveHandler dstate ps pr m c where
     ReceiveHandler :: MaybeDState dstate n
                    -> PeerReceiver ps pr (st :: ps) (st' :: ps) m c
                    -> ReceiveHandler dstate ps pr m c

-- | The handling of trailing data here is quite subtle. Trailing data is data
-- we have read from the channel but the decoder has told us that it comes
-- after the message we decoded. So it potentially belongs to the next message
-- to decode.
--
-- We read from the channel on both the 'runPipelinedPeerSender' and the
-- 'runPipelinedPeerReceiver', and we synchronise our use of trailing data
-- between the two. The scheme for the sender and receiver threads using the
-- channel ensures that only one can use it at once:
--
-- * When there are zero outstanding pipelined receiver handlers then the
--   sending side is allowed to access the channel directly (to do synchronous
--   yield\/awaits). Correspondingly the receiver side is idle and not
--   accessing the channel.
-- * When there are non-zero outstanding pipelined receiver handlers then
--   the receiver side can access the channel, but the sending side is not
--   permitted to do operations that access the channel.
--
-- So the only times we need to synchronise the trailing data are the times
-- when the right to access the channel passes from one side to the other.
--
-- The transitions are as follows:
--
-- * There having been Zero outstanding pipelined requests there is now a
--   new pipelined yield. In this case we must pass the trailing data from
--   the sender thread to the receiver thread. We pass it with the
--   'ReceiveHandler'.
--
-- * When the last pipelined request is collected. In this case we must pass
--   the trailing data from the receiver thread to the sender thread. We pass
--   it with the collected result.
--
-- Note that the receiver thread cannot know what the last pipelined request
-- is, that is tracked on the sender side. So the receiver thread always
-- returns the trailing data with every collected result. It is for the sender
-- thread to decide if it needs to use it. For the same reason, the receiver
-- thread ends up retaining the last trailing data (as well as passing it to
-- the sender). So correspondingly when new trailing data is passed to the
-- receiver thread, it simply overrides any trailing data it already had, since
-- we now know that copy to be stale.
--
data MaybeDState dstate (n :: N) where
     HasDState :: dstate -> MaybeDState dstate Z
     NoDState  ::           MaybeDState dstate (S n)


runPipelinedPeerSender
  :: forall ps (st :: ps) pr dstate c m a.
     ( MonadSTM    m
     , MonadThread m
     )
  => TQueue m (ReceiveHandler dstate ps pr m c)
  -> TQueue m (c, dstate)
  -> Driver ps dstate m
  -> PeerSender ps pr st Z c m a
  -> dstate
  -> m (a, dstate)
runPipelinedPeerSender receiveQueue collectQueue
                       Driver{sendMessage, recvMessage}
                       peer dstate0 = do
    threadId <- myThreadId
    labelThread threadId "pipeliend-peer-seneder"
    go Zero (HasDState dstate0) peer
  where
    go :: forall st' n.
          Nat n
       -> MaybeDState dstate n
       -> PeerSender ps pr st' n c m a
       -> m (a, dstate)
    go n    dstate             (SenderEffect k) = k >>= go n dstate
    go Zero (HasDState dstate) (SenderDone _ x) = return (x, dstate)

    go Zero dstate (SenderYield stok msg k) = do
      sendMessage stok msg
      go Zero dstate k

    go Zero (HasDState dstate) (SenderAwait stok k) = do
      (SomeMessage msg, dstate') <- recvMessage stok dstate
      go Zero (HasDState dstate') (k msg)

    go n dstate (SenderPipeline stok msg receiver k) = do
      atomically (writeTQueue receiveQueue (ReceiveHandler dstate receiver))
      sendMessage stok msg
      go (Succ n) NoDState k

    go (Succ n) NoDState (SenderCollect Nothing k) = do
      (c, dstate) <- atomically (readTQueue collectQueue)
      case n of
        Zero    -> go Zero      (HasDState dstate) (k c)
        Succ n' -> go (Succ n')  NoDState          (k c)

    go (Succ n) NoDState (SenderCollect (Just k') k) = do
      mc <- atomically (tryReadTQueue collectQueue)
      case mc of
        Nothing  -> go (Succ n) NoDState  k'
        Just (c, dstate) ->
          case n of
            Zero    -> go Zero      (HasDState dstate) (k c)
            Succ n' -> go (Succ n')  NoDState          (k c)


runPipelinedPeerReceiverQueue
  :: forall ps pr dstate m c.
     ( MonadSTM    m
     , MonadThread m
     )
  => TQueue m (ReceiveHandler dstate ps pr m c)
  -> TQueue m (c, dstate)
  -> Driver ps dstate m
  -> m Void
runPipelinedPeerReceiverQueue receiveQueue collectQueue
                              driver@Driver{startDState} = do
    threadId <- myThreadId
    labelThread threadId "pipelined-recevier-queue"
    go startDState
  where
    go :: dstate -> m Void
    go receiverDState = do
      ReceiveHandler senderDState receiver
        <- atomically (readTQueue receiveQueue)
      let dstate = case (senderDState, receiverDState) of
                       (HasDState t, _) -> t
                       (NoDState,    t) -> t
      x@(!_c, !dstate') <- runPipelinedPeerReceiver driver dstate receiver
      atomically (writeTQueue collectQueue x)
      go dstate'


runPipelinedPeerReceiver
  :: forall ps (st :: ps) (stdone :: ps) pr dstate m c.
     Monad m
  => Driver ps dstate m
  -> dstate
  -> PeerReceiver ps pr (st :: ps) (stdone :: ps) m c
  -> m (c, dstate)
runPipelinedPeerReceiver Driver{recvMessage} = go
  where
    go :: forall st' st''.
          dstate
       -> PeerReceiver ps pr st' st'' m c
       -> m (c, dstate)
    go dstate (ReceiverEffect k) = k >>= go dstate

    go dstate (ReceiverDone x) = return (x, dstate)

    go dstate (ReceiverAwait stok k) = do
      (SomeMessage msg, dstate') <- recvMessage stok dstate
      go dstate' (k msg)
