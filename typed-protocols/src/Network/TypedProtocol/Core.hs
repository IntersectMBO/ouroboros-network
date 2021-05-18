{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE QuantifiedConstraints #-}


-- | This module defines the core of the typed protocol framework.
--

module Network.TypedProtocol.Core (

  -- * Introduction
  -- $intro


  -- * Defining protocols
  -- $defining
  Protocol(..),
  -- $lemmas

  -- * Engaging in protocols
  -- $using
  PeerRole(..),
  TokPeerRole(..),
  FlipAgency,
  PeerHasAgency(..),
  WeHaveAgency,
  TheyHaveAgency,
  Peer(..),

  -- * Protocol proofs and tests
  -- $tests
  -- $lemmas
  ) where

import Data.Void (Void)

-- $intro
-- A typed protocol between two peers is defined via a state machine: a
-- collection of protocol states and protocol messages which are transitions
-- between those states.
--
-- Start from the idea that a protocol is some language of messages sent
-- between two peers. To specify a protocol is to describe what possible
-- sequences of messages are valid. One simple but still relatively expressive
-- way to do this is via a state machine: starting from some initial state,
-- all possible paths through the state machine gives the set of valid protocol
-- traces. This then dictates what a peer participating in a protocol may
-- produce and what it must accept.
--
-- In this style we have a fixed number of states and in each state there is
-- some number of valid messages that move us on to the next state. This can be
-- illustrated as a graph, which can be a helpful form of documentation.
--
-- We further constrain this idea by saying that the two peers will use the
-- same state machine and change states in lock-step by sending\/receiving
-- messages. In this approach, for each protocol state, the description
-- dictates which peer has the agency to choose to send a message, while
-- correspondingly the other must be prepared to receive the message.
--
-- The views of the two peers are dual. In each state one peer can send any
-- message that is valid for the current protocol state while the other
-- must be prepared to receive any valid message for current protocol state.
--
-- We can also have terminal protocol states in which neither peer has agency.
--
-- So part of the protocol description is to label each protocol state with
-- the peer that has the agency in that state, or none for terminal states.
-- We use the labels \"client\" and \"server\" for the two peers, but they are
-- in fact symmetric.


-- $defining
--
-- The 'Protocol' type class bundles up all the requirements for a typed
-- protocol, which are in fact all type level constructs. Defining a new
-- protocol and making it an instance of the 'Protocol' class requires the
-- following language extensions:
--
-- > {-# LANGUAGE GADTs, TypeFamilies, DataKinds #-}
--
-- The type class itself is indexed on a protocol \"tag\" type. This type
-- does double duty as the /kind/ of the /types/ of the protocol states.

-- We will use as a running example a simple \"ping\/pong\" protocol. (You can
-- see the example in full in "Network.TypedProtocol.PingPong.Type".) In this
-- example protocol the client sends a ping message and the serve must respond
-- with a pong message. The client can also terminate the protocol. So modelled
-- as a state machine this protocol has three states, the one in which the
-- client can send a ping or terminate message, the one in which the server
-- must send a pong, and the terminal state where neither can send anything.
-- We somewhat arbitrarily choose label these protocol states as \"idle\"
-- \"busy\" and \"done\".
--
-- For this ping pong example the protocol tag and the protocol state types
-- would be defined (via promoted data kinds) as:
--
-- > data PingPong where
-- >   StIdle :: PingPong
-- >   StBusy :: PingPong
-- >   StDone :: PingPong
--
-- We use @DataKinds@ promotion here so @StIdle@, @StBusy@ and @StDone@ are
-- /types/ (of /kind/ @PingPong@) representing the three states in this
-- protocol's state machine. @PingPong@ itself is both the kind of these types
-- and is also the tag for protocol. We only ever use these as types, via the
-- @DataKinds@ promotion, never as value level data constructors.
--
-- Having defined our protocol tag and states we can instantiate the 'Protocol'
-- type class and fill out the other details.
--
-- The protocol must define what its messages are. These form the state
-- transitions in the protocol state machine. Each transition specifies a
-- \"from\" and \"to\" state as type parameters. This of course determines in
-- which protocol states each message can appear.
--
-- In the \"ping\/pong\" protocol example, the messages are of course ping and
-- pong, which transition between the two main states. There is also a done
-- message that moves the system into a terminal state.
--
-- > instance Protocol PingPong where
-- >   data Message PingPong from to where
-- >     MsgPing :: Message PingPong StIdle StBusy
-- >     MsgPong :: Message PingPong StBusy StIdle
-- >     MsgDone :: Message PingPong StIdle StDone
--
-- This says that in the idle state a ping message takes us to the busy state,
-- while a pong message takes us back to idle. Also in the idle state a done
-- message takes us to the done state.
--
-- It is not required that protocols have any terminal states or corresponding
-- transitions, as in this example, but it is often useful and it aids testing
-- to have protocols that terminate cleanly as it allows them to return a
-- result.
--
-- As described above, this style of protocol gives agency to only one peer at
-- once. That is, in each protocol state, one peer has agency (the ability to
-- send) and the other does not (it must only receive). The three associated
-- data families ('ClientHasAgency', 'ServerHasAgency' and 'NobodyHasAgency')
-- define which peer has agency for each state.
--
-- In the \"ping\/pong\" protocol example, the idle state is the one in which
-- the client can send a message, and the busy state is the one in which the
-- server must respond. Finally in the done state, neither peer can send any
-- further messages. This arrangement is defined as so:
--
-- >    -- still within the instance Protocol PingPong
-- >    data ClientHasAgency st where
-- >      TokIdle :: ClientHasAgency StIdle
-- >
-- >    data ServerHasAgency st where
-- >      TokBusy :: ServerHasAgency StBusy
-- >
-- >    data NobodyHasAgency st where
-- >      TokDone :: NobodyHasAgency StDone
--
-- In this simple protocol there is exactly one state in each category, but in
-- general for non-trivial protocols there may be several protocol states in
-- each category.
--

-- $tests
-- The mechanism for labelling each protocol state with the agency does not
-- automatically prevent mislabelling, ie giving conflicting labels to a
-- single state. It does in fact prevent forgetting to label states in the
-- sense that it would not be possible to write protocol peers that make
-- progress having entered these unlabelled states.
--
-- This partition property is however crucial for the framework's guarantees.
-- The "Network.TypedProtocol.Proofs" module provides a way to guarantee for
-- each protocol that this property is not violated. It also provides utilities
-- helpful for testing protocols.

-- $lemmas
--
-- The 'connect' and 'connectPipelined' proofs rely on lemmas about the
-- protocol. Specifically they rely on the property that each protocol state
-- is labelled with the agency of one peer or the other, or neither, but never
-- both. Or to put it another way, the protocol states should be partitioned
-- into those with agency for one peer, or the other or neither.
--
-- The way the labelling is encoded does not automatically enforce this
-- property. It is technically possible to set up the labelling for a protocol
-- so that one state is labelled as having both peers with agency, or declaring
-- simultaneously that one peer has agency and that neither peer has agency
-- in a particular state.
--
-- So the overall proofs rely on lemmas that say that the labelling has been
-- done correctly. This type bundles up those three lemmas.
--
-- Specifically proofs that it is impossible for a protocol state to have:
--
-- * client having agency and server having agency
-- * client having agency and nobody having agency
-- * server having agency and nobody having agency
--
-- These lemmas are structured as proofs by contradiction, e.g. stating
-- \"if the client and the server have agency for this state then it leads to
-- contradiction\". Contradiction is represented as the 'Void' type that has
-- no values except ⊥.
--
-- For example for the ping\/pong protocol, it has three states, and if we set
-- up the labelling correctly we have:
--
-- > data PingPong where
-- >   StIdle :: PingPong
-- >   StBusy :: PingPong
-- >   StDone :: PingPong
-- >
-- > instance Protocol PingPong where
-- >
-- >   data ClientHasAgency st where
-- >     TokIdle :: ClientHasAgency StIdle
-- >
-- >   data ServerHasAgency st where
-- >     TokBusy :: ServerHasAgency StBusy
-- >
-- >   data NobodyHasAgency st where
-- >     TokDone :: NobodyHasAgency StDone
--
-- So now we can prove that if the client has agency for a state then there
-- are no cases in which the server has agency.
--
-- >   exclusionLemma_ClientAndServerHaveAgency TokIdle tok = case tok of {}
--
-- For this protocol there is only one state in which the client has agency,
-- the idle state. By pattern matching on the state token for the server
-- agency we can list all the cases in which the server also has agency for
-- the idle state. There are of course none of these so we give the empty
-- set of patterns. GHC can check that we are indeed correct about this.
-- This also requires the @EmptyCase@ language extension.
--
-- To get this completeness checking it is important to compile modules
-- containing these lemmas with @-Wincomplete-patterns@, which is implied by
-- @-Wall@.
--
-- All three lemmas follow the same pattern.
--

-- | The protocol type class bundles up all the requirements for a typed
-- protocol.
--
-- Each protocol consists of three things:
--
-- * The protocol itself, which is also expected to be the kind of the types
--   of the protocol states. The class is indexed on the protocol itself.
-- * The protocol messages.
-- * The partition of the protocol states into those in which the client has
--   agency, or the server has agency, or neither have agency.
--
-- The labelling of each protocol state with the peer that has agency in that
-- state is done by giving a definition to the data families 
-- 'ClientHasAgency', 'ServerHasAgency' and 'NobodyHasAgency'. These
-- definitions are expected to be singleton-style GADTs with one constructor
-- per protocol state.
--
-- Each protocol state must be assigned to only one label. See
-- "Network.TypedProtocol.Proofs" for more details on this point.
--
class Protocol ps where

  -- | The messages for this protocol. It is expected to be a GADT that is
  -- indexed by the @from@ and @to@ protocol states. That is the protocol state
  -- the message transitions from, and the protocol state it transitions into.
  -- These are the edges of the protocol state transition system.
  --
  data Message ps (st :: ps) (st' :: ps)

  -- | Tokens for those protocol states in which the client has agency.
  --
  data ClientHasAgency (st :: ps)

  -- | Tokens for those protocol states in which the server has agency.
  --
  data ServerHasAgency (st :: ps)

  -- | Tokens for terminal protocol states in which neither the client nor
  -- server has agency.
  --
  data NobodyHasAgency (st :: ps)

  -- | Lemma that if the client has agency for a state, there are no
  -- cases in which the server has agency for the same state.
  --
  exclusionLemma_ClientAndServerHaveAgency
    :: forall (st :: ps).
       ClientHasAgency st
    -> ServerHasAgency st
    -> Void

  -- | Lemma that if the nobody has agency for a state, there are no
  -- cases in which the client has agency for the same state.
  --
  exclusionLemma_NobodyAndClientHaveAgency
    :: forall (st :: ps).
       NobodyHasAgency st
    -> ClientHasAgency st
    -> Void

  -- | Lemma that if the nobody has agency for a state, there are no
  -- cases in which the server has agency for the same state.
  --
  exclusionLemma_NobodyAndServerHaveAgency
    :: forall (st :: ps).
       NobodyHasAgency st
    -> ServerHasAgency st
    -> Void

-- | Types for client and server peer roles. As protocol can be viewed from
-- either client or server side.
--
-- Note that technically \"client\" and \"server\" are arbitrary labels. The
-- framework is completely symmetric between the two peers.
--
-- This definition is only used as promoted types and kinds, never as values.
--
data PeerRole = AsClient | AsServer

-- | Singletons for the promoted 'PeerRole' types.  Not directly used by the
-- framework, however some times useful when writing code that is shared between
-- client and server.
--
data TokPeerRole (peerRole :: PeerRole) where
    TokAsClient :: TokPeerRole AsClient
    TokAsServer :: TokPeerRole AsServer

-- | This data type is used to hold state tokens for states with either client
-- or server agency. This GADT shows up when writing protocol peers, when
-- 'Yield'ing or 'Await'ing, and when writing message encoders\/decoders.
--
data PeerHasAgency (pr :: PeerRole) (st :: ps) where
  ClientAgency :: !(ClientHasAgency st) -> PeerHasAgency AsClient st
  ServerAgency :: !(ServerHasAgency st) -> PeerHasAgency AsServer st

instance ( forall (st' :: ps). Show (ClientHasAgency st')
         , forall (st' :: ps). Show (ServerHasAgency st')
         ) => Show (PeerHasAgency pr (st :: ps)) where
    show (ClientAgency stok) = "ClientAgency " ++ show stok
    show (ServerAgency stok) = "ServerAgency " ++ show stok

-- | A synonym for an state token in which \"our\" peer has agency. This is
-- parametrised over the client or server roles. In either case the peer in
-- question has agency.
--
-- This shows up when we are sending messages, or dealing with encoding
-- outgoing messages.
--
type WeHaveAgency   (pr :: PeerRole) st = PeerHasAgency             pr  st

-- | A synonym for an state token in which the other peer has agency. This is
-- parametrised over the client or server roles. In either case the other peer
-- has agency.
--
-- This shows up when we are receiving messages, or dealing with decoding
-- incoming messages.
--
type TheyHaveAgency (pr :: PeerRole) st = PeerHasAgency (FlipAgency pr) st

-- | A type function to flip the client and server roles.
--
type family FlipAgency (pr :: PeerRole) where
  FlipAgency AsClient = AsServer
  FlipAgency AsServer = AsClient


-- | A description of a peer that engages in a protocol.
--
-- The protocol describes what messages peers /may/ send or /must/ accept.
-- A particular peer implementation decides what to actually do within the
-- constraints of the protocol.
--
-- Peers engage in a protocol in either the client or server role. Of course
-- the client role can only interact with the serve role for the same protocol
-- and vice versa.
--
-- 'Peer' has several type arguments:
--
-- * the protocol itself;
-- * the client\/server role;
-- *.the current protocol state;
-- * the monad in which the peer operates; and
-- * the type of any final result once the peer terminates.
--
-- For example:
--
-- > pingPongClientExample :: Int -> Peer PingPong AsClient StIdle m ()
-- > pingPongServerExample ::        Peer PingPong AsServer StIdle m Int
--
-- The actions that a peer can take are:
--
-- * to perform local monadic effects
-- * to terminate with a result (but only in a terminal protocol state)
-- * to send a message (but only in a protocol state in which we have agency)
-- * to wait to receive a message (but only in a protocol state in which the
--   other peer has agency)
--
-- In the 'Done', 'Yield' and 'Await' cases we must provide evidence of both
-- the protocol state we are in and that the appropriate peer has agency.
-- This takes the form of 'ClientAgency' or 'ServerAgency' applied to a
-- protocol-specific state token: either a 'ClientHasAgency' or a
-- 'ServerHasAgency' token for the protocol. The 'Done' state does not need
-- the extra agency information.
--
-- While this evidence must be provided, the types guarantee that it is not
-- possible to supply incorrect evidence.
--
data Peer ps (pr :: PeerRole) (st :: ps) m a where

  -- | Perform a local monadic effect and then continue.
  --
  -- Example:
  --
  -- > Effect $ do
  -- >   ...          -- actions in the monad
  -- >   return $ ... -- another Peer value
  --
  Effect :: m (Peer ps pr st m a)
         ->    Peer ps pr st m a

  -- | Terminate with a result. A state token must be provided from the
  -- 'NobodyHasAgency' states, so show that this is a state in which we can
  -- terminate.
  --
  -- Example:
  --
  -- > Yield (ClientAgency TokIdle)
  -- >        MsgDone
  -- >       (Done TokDone result)
  --
  Done   :: !(NobodyHasAgency st)
         -> a
         -> Peer ps pr st m a

  -- | Send a message to the other peer and then continue. This takes the
  -- message and the continuation. It also requires evidence that we have
  -- agency for this protocol state and thus are allowed to send messages.
  --
  -- Example:
  --
  -- > Yield (ClientAgency TokIdle) MsgPing $ ...
  --
  Yield  :: !(WeHaveAgency pr st)
         -> Message ps st st'
         -> Peer ps pr st' m a
         -> Peer ps pr st  m a

  -- | Waits to receive a message from the other peer and then continues.
  -- This takes the the continuation that is supplied with the received
  -- message. It also requires evidence that the other peer has agency for
  -- this protocol state and thus we are expected to wait to receive messages.
  --
  -- Note that the continuation that gets supplied with the message must be
  -- prepared to deal with /any/ message that is allowed in /this/ protocol
  -- state. This is why the continuation /must/ be polymorphic in the target
  -- state of the message (the third type argument of 'Message').
  --
  -- Example:
  --
  -- > Await (ClientAgency TokIdle) $ \msg ->
  -- > case msg of
  -- >   MsgDone -> ...
  -- >   MsgPing -> ...
  --
  Await  :: !(TheyHaveAgency pr st)
         -> (forall st'. Message ps st st' -> Peer ps pr st' m a)
         -> Peer ps pr st m a


deriving instance Functor m => Functor (Peer ps (pr :: PeerRole) (st :: ps) m)
