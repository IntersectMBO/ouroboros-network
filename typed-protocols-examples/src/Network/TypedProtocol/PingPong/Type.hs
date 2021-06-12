{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE EmptyCase          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}


module Network.TypedProtocol.PingPong.Type where

import           Data.Singletons

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

data SPingPong (st :: PingPong) where
  SingIdle :: SPingPong StIdle
  SingBusy :: SPingPong StBusy
  SingDone :: SPingPong StDone

deriving instance Show (SPingPong st)

type instance Sing = SPingPong
instance SingI StIdle where
    sing = SingIdle
instance SingI StBusy where
    sing = SingBusy
instance SingI StDone where
    sing = SingDone


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

  type StateAgency StIdle = ClientAgency
  type StateAgency StBusy = ServerAgency
  type StateAgency StDone = NobodyAgency


deriving instance Show (Message PingPong from to)
