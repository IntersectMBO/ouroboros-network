{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

--TODO: just temporary while hacking:
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-incomplete-patterns #-}

-- |
-- = Driving a Peer by was of a Duplex and Channel
--
-- A 'Duplex' allows for sending and receiving pieces of some concrete type.
-- In applications, this will probably be some sort of socket. In order to
-- use it to drive a typed protocol application (represented by a 'Peer'),
-- there must be a way to encode typed transitions of that protocol to the
-- concrete type, and to parse pieces of that concrete type incrementally into
-- a typed transition. This is defined by a 'Codec'.
--
-- A 'Codec' and a 'Duplex' alone is not enough to do encoding and decoding,
-- because the 'Codec' does not make any /decisions/ about the way in which
-- the protocol application progresses. It defines encodings for /all/ possible
-- transitions from a state, and an inverse for that encoder. It's the 'Peer'
-- term which decides which transitions to encode, thereby leading the 'Codec'
-- through a path in the protocol type.
--
-- Driving a 'Peer' in this way may give rise to an exception, given by
-- @'Unexpected' :: 'Result' t@.

module Network.TypedProtocol.Driver where

import Network.TypedProtocol.Core      as Core
--import Network.TypedProtocol.Pipelined as Pipelined
import Network.TypedProtocol.Channel
import Network.TypedProtocol.Codec     as Codec

--import           Control.Monad.Class.MonadSTM


-- | The 'connect' function takes two peers that agree on a protocol and runs
-- them in lock step, until (and if) they complete.
--
-- The 'connect' function serves a few purposes.
--
-- * The fact we can define this function at at all proves some minimal
-- sanity property of the typed protocol framework.
--
-- * It demonstrates that all protocols defined in the framework can be run
-- with synchronous communication rather than requiring buffered communication.
--
-- * It is useful for testing peer implementations against each other in a
-- minimalistic setting. The typed framework guarantees
--
connect :: Monad m
        => Peer AsClient st m a
        -> Peer AsServer st m b
        -> m (a, b)
connect (Core.Effect a) b  = a >>= \a' -> connect a' b
connect a  (Core.Effect b) = b >>= \b' -> connect a  b'
connect (Core.Done a) (Core.Done b) = return (a, b)
connect (Core.Yield msg a) (Core.Await _ b) = connect a (b msg)
connect (Core.Await _ a) (Core.Yield msg b) = connect (a msg) b
--connect (Done _) (Yield _ _) = 



runPeer :: forall ps (st :: ps) pk failure bytes m a .
           Monad m
        => Codec ps failure m bytes
        -> Channel m bytes
        -> Peer pk st m a
        -> m a
runPeer codec channel (Core.Effect k) =
    k >>= runPeer codec channel

runPeer _codec _channel (Core.Done x) =
    return x

runPeer codec@Codec{encode} Channel{send} (Core.Yield msg k) = do
    let msgbytes :: bytes
        msgbytes = encode msg
    channel' <- send msgbytes
    runPeer codec channel' k

runPeer codec@Codec{decode} channel (Core.Await stok k) = do
    decoder <- decode stok
    res <- runDecoder channel decoder
    case res of
      Right (SomeMessage msg, channel') -> runPeer codec channel' (k msg)
      Left failure                      -> undefined

runDecoder :: Monad m
           => Channel m bytes
           -> Codec.DecodeStep bytes failure m a
           -> m (Either failure (a, Channel m bytes))
runDecoder channel (Codec.Done x trailing) = return (Right (x, channel)) --TODO: prepend trailing

runDecoder _channel (Codec.Fail failure) = return (Left failure)

runDecoder Channel{recv} (Codec.Partial k) = do
    (minput, channel') <- recv
    runDecoder channel' =<< k minput

