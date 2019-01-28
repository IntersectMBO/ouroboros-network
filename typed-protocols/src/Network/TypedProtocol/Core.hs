{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}


-- | This module defines the core of the typed protocol framework.
--
-- The typed protocol framework is used to define, test and execute protocols.
-- It also provides some abstractions for untyped channels and for encoding and
-- decoding protocol messages from untyped channels.
--
-- The typed protocol framework guarantees:
--
-- * agreement on which messages can be sent and received;
-- * the absence of race conditions; and
-- * the absence of deadlock.
--
-- It is a simple form of session typing. The trade-off to achieve these
-- guarantees it that places constraints on the kinds of protocol that can be
-- expressed. In particular it requires that protocols be defined as a state
-- transition system. It requires for each protocol state that one of the two
-- peers be able to send and the other must be ready to receive.
--
-- This means it is not possible to express protocols such as TCP where there
-- are protocol states where a single peer can both send and receive, however
-- it is suitable for most application-level protocols. In particular many
-- application-level protocols are completely in-order and synchronous.
--
-- In many (but not all) cases it is also possible to pipeline these protocols
-- so that network latency can be hidden and full use made of the available
-- bandwidth. Special support is provided to run protocols in a pipelined way,
-- without having to change the protocol definition.
--
-- The protocols in this framework assume an underlying \"reliable ordered\"
-- connection. A \"reliable ordered\" connection is a term of art means one
-- where the receiving end receives any prefix of the messages sent by the
-- sending end. It is not reliable in the colloquial sense as it does not
-- ensure that anything actually arrives, only that /if/ any message arrives,
-- all the previous ones did too, and in the order in which they were sent.
--
module Network.TypedProtocol.Core (
  Protocol(..),
  Peer(..),
  PeerKind(..),
  WeHaveAgency,
  TheyHaveAgency,
  ) where

import Data.Kind (Type)


-- | A typed protocol between two peers is defined via a state machine: a
-- collection of protocol states and protocol messages which are transitions
-- between those states.
--
-- This type class bundles up all the requirements for a typed protocol, which
-- are in fact all type level constructs. Defining a new protocol and making it
-- an instance of this class requires the following language extensions:
--
-- > {-# LANGUAGE GADTs, TypeFamilies, RankNTypes, PolyKinds, DataKinds #-}
--
-- The type class itself is indexed on the protocol's state type. For example
-- for a simple \"ping\/pong\" protocol it would be defined (via promoted data
-- kinds) as:
--
-- > data PingPongState where
-- >   StIdle :: PingPongState
-- >   StBusy :: PingPongState
-- >   StDone :: PingPongState
--
-- This style of protocol gives agency to only one peer at once. That is, in
-- each protocol state, one peer has agency (the ability to send) and the other
-- does not (it can only receive). The 'AgencyInState' type family defines
-- for each state which peer has agency.
--
-- In the \"ping\/pong\" protocol example, the idle state is the one in which
-- the client can send a message, and the busy state is the one in which the
-- server must respond. Finally in the done state, neither peer can send any
-- further messages. This arrangement is defined within the type class as so:
--
-- > instance Protocol PingPongState where
-- >   type AgencyInState StIdle = ClientHasAgency
-- >   type AgencyInState StBusy = ServerHasAgency
-- >   type AgencyInState StDone = NobodyHasAgency
--
-- The labels client and server are arbitrary and in fact all such protocols
-- are completely symmetric between the two peers.
--
-- Next, the protocol must define what its state transitions are. These form
-- the messages of the protocol. The transitions determine what protocol states
-- the go from and to, which of course determines in which protocol states each
-- message can appear.
--
-- In the \"ping\/pong\" protocol example, the messages are of course ping and
-- pong, which transition between the two main states. There is also a done
-- message that moves the system into a terminal state.
--
-- >   -- still within the instance Protocol PingPongState
-- >   data Message from to where
-- >     MsgPing :: Message StIdle StBusy
-- >     MsgPong :: Message StBusy StIdle
-- >     MsgDone :: Message StIdle StDone
--
-- It is not required that protocols have any terminal states or transitions,
-- as in this example, but it is often useful and it aids testing to have
-- protocols that terminate.
--
-- Finally, for technical reasons it is necessary to define a value level
-- representation of a protocol state that matches up with the type level
-- definition of the protocol states. This is also known as a singleton type.
-- 
-- The representation is as a GADT and follows mechanically from the structure
-- of the protocol state type. For the  \"ping\/pong\" protocol example it
-- looks like so:
--
-- >   -- still within the instance Protocol PingPongState
-- >   data StateToken st where
-- >     TokIdle :: StateToken StIdle
-- >     TokBusy :: StateToken StBusy
-- >     TokDone :: StateToken StDone
--
class Protocol ps where

  -- | The protocol message type for this protocol. It is expected to be a
  -- GADT that is indexed by the @from@ and @to@ protocol states. That is the
  -- protocol state the message transitions from, and the protocol state it
  -- transitions into. These are the edges of the protocol state transition
  -- system.
  --
  data Message ps (st :: ps) (st' :: ps)

  -- | A type for the value level representation of a protocol state. This is
  -- a GADT singleton that reflects each protocol state as a value. This is
  -- used message decoders to determine what state the protocol is in and thus
  -- what messages are permissible. It is also used in pipelined protocol
  -- interpreters as part of dynamically keeping track of the expected
  -- outstanding responses and their protocol states.
  --

  -- | The peer that has the agency in each protocol state, which is either
  -- the client, or the server (or neither for terminal states).
  --

  data ClientHasAgency (st :: ps)
  data ServerHasAgency (st :: ps)
  data NobodyHasAgency (st :: ps)


data PeerKind = AsClient | AsServer        -- Only used as promoted types

type family WeHaveAgency (pk :: PeerKind) st :: Type where
  WeHaveAgency AsClient st = ClientHasAgency st
  WeHaveAgency AsServer st = ServerHasAgency st

type family TheyHaveAgency (pk :: PeerKind) st :: Type where
  TheyHaveAgency AsClient st = ServerHasAgency st
  TheyHaveAgency AsServer st = ClientHasAgency st


-- | Having defined the types needed for a protocol it is then possible to
-- define programs that are peers that engage in that protocol.
--
data Peer (pk :: PeerKind) (st :: ps) m a where

  Effect :: m (Peer pk st m a)
         ->    Peer pk st m a

  Done   :: NobodyHasAgency st
         -> a
         -> Peer pk st m a

  Yield  :: WeHaveAgency pk st
         -> Message ps st st'
         -> Peer pk st' m a
         -> Peer pk st  m a

  Await  :: TheyHaveAgency pk st
         -> (forall st'. Message ps st st' -> Peer pk st' m a)
         -> Peer pk st m a

