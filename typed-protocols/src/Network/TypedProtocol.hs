
-- | This package defines the typed protocol framework. This module re-exports
-- the public API.
--
module Network.TypedProtocol
  ( -- * Introduction
    -- $intro
    -- * Defining and implementing protocols
    -- $defining
    module Network.TypedProtocol.Core
    -- ** Protocol proofs and tests
    -- $tests
  , module Network.TypedProtocol.Proofs
    -- * Running protocols
    -- $running
  , module Network.TypedProtocol.Driver
    -- * Pipelining protocols
    -- $pipelining
  , module Network.TypedProtocol.Pipelined
  ) where

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Driver
import           Network.TypedProtocol.Pipelined
import           Network.TypedProtocol.Proofs


-- $intro
--
-- The typed protocol framework is used to define, test and execute protocols.
--
-- It guarantees:
--
-- * agreement on which messages can be sent and received;
-- * the absence of race conditions; and
-- * the absence of deadlock.
--
-- The trade-off to achieve these guarantees is that it places constraints on
-- the kinds of protocol that can be expressed. In particular it requires that
-- protocols be defined as a state transition system. It requires for each
-- protocol state that exactly one of the two peers be able to send and the
-- other must be ready to receive.
--
-- This means it is not possible to express protocols such as TCP where there
-- are protocol states where a single peer can both send and receive, however
-- it is suitable for most application-level protocols. In particular many
-- application-level protocols are completely in-order and synchronous. That
-- said, in many (but not all) cases it is possible to pipeline these protocols
-- so that network latency can be hidden and full use made of the available
-- bandwidth. Special support is provided to run protocols in a pipelined way,
-- without having to change the protocol definition.
--
-- The protocols in this framework assume an underlying \"reliable ordered\"
-- connection. A \"reliable ordered\" connection is a term of art meaning one
-- where the receiving end receives any prefix of the messages sent by the
-- sending end. It is not reliable in the colloquial sense as it does not
-- ensure that anything actually arrives, only that /if/ any message arrives,
-- all the previous messages did too, and that they arrive in the order in
-- which they were sent.
--
-- The framework also provides:
--
-- * an abstraction for untyped channels;
-- * a codec abstraction for encoding and decoding protocol messages; and
-- * drivers for running protocol peers with a channel and a codec.


-- $defining
--
-- The "Network.TypedProtocol.Core" module defines the core of the system.
--
-- Start reading here to understand:
--
--  * how to define new protocols; or
--  * to write peers that engage in a protocol.
--
-- Typed protocol messages need to be converted to and from untyped
-- serialised forms to send over a transport channel. So part of defining a new
-- protocol is to define the message encoding and the codec for doing the
-- encoding and decoding. This is somewhat (but not significantly) more complex
-- than defining normal data type serialisation because of the need to decode
-- typed protocol messages. The "Network.TypedProtocol.Codec" module provides
-- the codec abstraction to capture this.


-- $tests
--
-- There are a few proofs about the framework that we can state and implement
-- as Haskell functions (using GADTs and evaluation). A couple of these proofs
-- rely on a few lemmas that should be proved for each protocol. The
-- "Network.TypedProtocol.Proofs" module describes these proof and provides
-- the infrastructure for the simple lemmas that need to be implemented for
-- each protocol.
--
-- This module also provides utilities helpful for testing protocols.


-- $running
--
-- Typed protocols need to be able to send messages over untyped transport
-- channels. The "Network.TypedProtocol.Channel" module provides such an
-- abstraction. You can use existing example implementations of this interface
-- or define your own to run over other transports.
--
-- Given a protocol peer, and a channel and a codec we can run the protocol
-- peer so that it engages in the protocol sending and receiving messages
-- over the channel. The "Network.TypedProtocol.Driver" module provides drivers
-- for normal and pipelined peers.


-- $pipelining
-- Protocol pipelining is a technique to make effective use of network
-- resources.
--
-- <<https://upload.wikimedia.org/wikipedia/commons/1/19/HTTP_pipelining2.svg>>
--
-- As in the above diagram, instead of sending a request and waiting for the
-- response before sending the next request, pipelining involves sending all
-- three requests back-to-back and waiting for the three replies. The server
-- still simply processes the requests in order and the replies come back in
-- the same order as the requests were made.
--
-- Not only does this save network latency, one round trip versus three in
-- the diagram above, but it also makes effective use of the bandwidth by
-- sending requests and replies back-to-back.
--
-- In the example in the diagram it stops after three requests, but such a
-- pattern can go on indefinately with messages going in both directions,
-- which can saturate the available bandwidth.
--
-- For many (but not all) protocols that can be defined in the @typed-protocol@
-- framework it is possible to take the protocol, without changing the
-- protocol's state  machine, and to engage in the protocol in a pipelined way.
-- Only the pipelined client has to be written specially. The server side can
-- be used unaltered and can be used with either pipelined or non-pipelined
-- clients.


