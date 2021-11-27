{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Network.TypedProtocol.PingPong.Client
  ( -- * Normal client
    PingPongClient (..)
  , pingPongClientPeer
    -- * Pipelined client
  , PingPongClientPipelined (..)
  , PingPongClientIdle (..)
  , pingPongClientPeerPipelined
  ) where

import           Control.Monad.Class.MonadSTM (STM)

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Peer.Client
import           Network.TypedProtocol.PingPong.Type

-- | A ping-pong client, on top of some effect 'm'.
--
-- At each step the client has a choice: ping or stop.
--
-- This type encodes the pattern of state transitions the client can go through.
-- For the ping\/pong case this is trivial. We start from one main state,
-- issue a ping and move into a state where we expect a single response,
-- bringing us back to the same main state.
--
-- If we had another state in which a different set of options were available
-- then we would need a second type like this. The two would be mutually
-- recursive if we can get in both directions, or perhaps just one way such
-- as a special initialising state or special terminating state.
--
data PingPongClient m a where
  -- | Choose to go for sending a ping message. The ping has no body so
  -- all we have to provide here is a continuation for the single legal
  -- reply message.
  --
  SendMsgPing    :: m (PingPongClient m a) -- continuation for Pong response
                 -> PingPongClient m a

  -- | Choose to terminate the protocol. This is an actual but nullary message,
  -- we terminate with the local result value. So this ends up being much like
  -- 'return' in this case, but in general the termination is a message that
  -- can communicate final information.
  --
  SendMsgDone    :: a -> PingPongClient m a


-- | Interpret a particular client action sequence into the client side of the
-- 'PingPong' protocol.
--
pingPongClientPeer
  :: Monad m
  => PingPongClient m a
  -> Client PingPong NonPipelined Empty StIdle m stm a

pingPongClientPeer (SendMsgDone result) =
    -- We do an actual transition using 'yield', to go from the 'StIdle' to
    -- 'StDone' state. Once in the 'StDone' state we can actually stop using
    -- 'done', with a return value.
    Yield MsgDone (Done result)

pingPongClientPeer (SendMsgPing next) =

    -- Send our message.
    Yield MsgPing $

    -- The type of our protocol means that we're now into the 'StBusy' state
    -- and the only thing we can do next is local effects or wait for a reply.
    -- We'll wait for a reply.
    Await $ \MsgPong ->

    -- Now in this case there is only one possible response, and we have
    -- one corresponding continuation 'kPong' to handle that response.
    -- The pong reply has no content so there's nothing to pass to our
    -- continuation, but if there were we would.
      Effect $ do
        client <- next
        pure $ pingPongClientPeer client


--
-- Pipelined client
--

-- | A ping-pong client designed for running the 'PingPong' protocol in
-- a pipelined way.
--
data PingPongClientPipelined m a where
  -- | A 'PingPongSender', but starting with zero outstanding pipelined
  -- responses, and for any internal collect type @c@.
  PingPongClientPipelined ::
      PingPongClientIdle      Empty m a
   -> PingPongClientPipelined       m a


data PingPongClientIdle (q :: Queue PingPong) m a where
  -- | Send a `Ping` message but alike in `PingPongClient` do not await for the
  -- response, instead supply a monadic action which will run on a received
  -- `Pong` message.
  --
  SendMsgPingPipelined
    :: PingPongClientIdle (q |> Tr StBusy StIdle) m a -- continuation
    -> PingPongClientIdle  q                      m a

  -- | Collect the result of a previous pipelined receive action.
  --
  -- This (optionally) provides two choices:
  --
  -- * Continue without a pipelined result
  -- * Continue with a pipelined result, which allows to run a monadic action
  --   when 'MsgPong' is received.
  --
  -- Since presenting the first choice is optional, this allows expressing
  -- both a blocking collect and a non-blocking collect. This allows
  -- implementations to express policies such as sending a short sequence
  -- of messages and then waiting for all replies, but also a maximum pipelining
  -- policy that keeps a large number of messages in flight but collects results
  -- eagerly.
  --
  CollectPipelined
    :: Maybe (PingPongClientIdle (Tr StBusy StIdle <| q) m a)
    -> m     (PingPongClientIdle                      q  m a)
    ->        PingPongClientIdle (Tr StBusy StIdle <| q) m a

  -- | Termination of the ping-pong protocol.
  --
  -- Note that all pipelined results must be collected before terminating.
  --
  SendMsgDonePipelined
    :: a -> PingPongClientIdle Empty m a



-- | Interpret a pipelined client as a pipelined 'Peer' on the client side of
-- the 'PingPong' protocol.
--
pingPongClientPeerPipelined
  :: Functor m
  => PingPongClientPipelined m a
  -> Client PingPong 'Pipelined Empty StIdle m (STM m) a
pingPongClientPeerPipelined (PingPongClientPipelined peer) =
    pingPongClientPeerIdle peer


pingPongClientPeerIdle
  :: forall (q :: Queue PingPong) m a. Functor m
  => PingPongClientIdle                q        m a
  -> Client PingPong 'Pipelined q StIdle m (STM m) a
pingPongClientPeerIdle = go
  where
    go :: forall (q' :: Queue PingPong).
          PingPongClientIdle         q'        m         a
       -> Client PingPong 'Pipelined q' StIdle m (STM m) a

    go (SendMsgPingPipelined next) =
      -- Pipelined yield: send `MsgPing`, immediately follow with the next step.
      YieldPipelined
        MsgPing
        (go next)

    go (CollectPipelined mNone collect) =
      Collect
        (go <$> mNone)
        (\MsgPong -> CollectDone $ Effect (go <$> collect))

    go (SendMsgDonePipelined result) =
      -- Send `MsgDone` and complete the protocol
      Yield
        MsgDone
        (Done result)
