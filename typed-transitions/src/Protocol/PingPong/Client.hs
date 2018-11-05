{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

{-# OPTIONS_GHC "-fwarn-incomplete-patterns" #-}

module Protocol.PingPong.Client where

import Protocol.Core
import Protocol.PingPong.Type


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
  SendMsgStop    :: a -> PingPongClient m a


-- | Interpret a particular client action sequence into the client side of the
-- 'PingPong' protocol.
--
pingPongClientPeer
  :: Monad m
  => PingPongClient m a
  -> Peer PingPongProtocol PingPongMessage
          (Yielding StIdle) (Finished StDone)
          m a

pingPongClientPeer (SendMsgStop result) =
    -- We do an actual transition using 'out', meaning sending a message, to go
    -- from the 'StIdle' to 'StDone' state. Once in the 'StDone' state we can
    -- actually stop using 'done', with a return value.
    out MsgDone (done result)

pingPongClientPeer (SendMsgPing kPong) =

    -- Send our message with 'over'.
    over MsgPing $

    -- The type of our protocol means that we're now into the 'StBusy' state
    -- and the only thing we can do next is local effects or wait for a reply.
    -- We'll wait for a reply.
    await $ \resp ->

    -- Now in this case there is only one possible response, and we have
    -- one corresponding continuation 'kPong' to handle that response.
    -- The pong reply has no content so there's nothing to pass to our
    -- continuation, but if there were we would.
    case resp of
      MsgPong -> lift $ do
        next <- kPong
        pure $ pingPongClientPeer next

