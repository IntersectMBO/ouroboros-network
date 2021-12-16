{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE EmptyCase          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}


module Network.TypedProtocol.PingPong.Type where

import           Network.TypedProtocol.Core


-- | The ping\/pong protocol and the states in its protocol state machine.
--
-- This protocol serves as a simple example of the typed protocols framework
-- to help understand the framework and as a template for writing other
-- protocols.
--
-- For a slightly more realistic example, see the request\/response protocol
-- "Network.TypedProtocol.ResResp.Type".
--
-- This declares the protocol itself. It is used both as a type level tag for
-- the protocol and as the kind of the types of the states in the protocol
-- state machine. That is @PingPong@ is a kind, and @StIdle@ is a type of
-- that kind.
--
-- If the protocol needs any type parameters (e.g. for thing that end up in
-- the messages) then those type parameters go here. See the request\/response
-- protocol for an example. It is parametrised over the types of the request
-- and response.
--
data PingPong where
  StIdle :: PingPong
  StBusy :: PingPong
  StDone :: PingPong

instance Protocol PingPong where

  -- | The actual messages in our protocol.
  --
  -- These involve transitions between different states within the 'PingPong'
  -- states. A ping request goes from idle to busy, and a pong response go from
  -- busy to idle.
  --
  -- This example is so simple that we have all the messages directly as
  -- constructors within this type. In more complex cases it may be better to
  -- factor all (or related) requests and all responses within one case (in
  -- which case the state transitions may depend on the particular message via
  -- the usual GADT tricks).
  --
  data Message PingPong from to where
    MsgPing :: Message PingPong StIdle StBusy
    MsgPong :: Message PingPong StBusy StIdle
    MsgDone :: Message PingPong StIdle StDone

  -- | We have to explain to the framework what our states mean, in terms of
  -- who is expected to send and receive in the different states.
  --
  -- Idle states are where it is for the client to send a message.
  --
  data ClientHasAgency st where
    TokIdle :: ClientHasAgency StIdle

  -- | Busy states are where the server is expected to send a reply (a pong).
  --
  data ServerHasAgency st where
    TokBusy :: ServerHasAgency StBusy

  -- | In the done state neither client nor server can send messages.
  --
  data NobodyHasAgency st where
    TokDone :: NobodyHasAgency StDone

  exclusionLemma_ClientAndServerHaveAgency TokIdle tok = case tok of {}
  exclusionLemma_NobodyAndClientHaveAgency TokDone tok = case tok of {}
  exclusionLemma_NobodyAndServerHaveAgency TokDone tok = case tok of {}


deriving instance Show (Message PingPong from to)

instance Show (ClientHasAgency (st :: PingPong)) where
  show TokIdle = "TokIdle"

instance Show (ServerHasAgency (st :: PingPong)) where
  show TokBusy = "TokBusy"
