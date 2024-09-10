{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- @UndecidableInstances@ extensions is required for defining @Show@ instance
-- of @'TraceSendRecv'@.
{-# LANGUAGE UndecidableInstances  #-}

-- | Drivers for running 'Peer's with a 'Codec' and a 'Channel'.
--
module Ouroboros.Network.Driver.Simple
  ( -- * Introduction
    -- $intro
    -- * Normal peers
    runPeer
  , runAnnotatedPeer
  , TraceSendRecv (..)
  , DecoderFailure (..)
    -- * Pipelined peers
  , runPipelinedPeer
  , runPipelinedAnnotatedPeer
    -- * Connected peers
    -- TODO: move these to a test lib
  , Role (..)
  , runConnectedPeers
  , runConnectedPeersAsymmetric
  , runConnectedPeersPipelined
  ) where

import Network.TypedProtocol.Codec
import Network.TypedProtocol.Core
import Network.TypedProtocol.Driver
import Network.TypedProtocol.Pipelined

import Ouroboros.Network.Util.ShowProxy

import Ouroboros.Network.Channel

import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadThrow
import Control.Tracer (Tracer (..), contramap, traceWith)
import Data.Maybe (fromMaybe)
import Data.Functor.Identity (Identity)
import Control.Monad.Identity (Identity(..))


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

-- | Structured 'Tracer' output for 'runPeer' and derivitives.
--
data TraceSendRecv ps where
     TraceSendMsg :: AnyMessageAndAgency ps -> TraceSendRecv ps
     TraceRecvMsg :: AnyMessageAndAgency ps -> TraceSendRecv ps

instance Show (AnyMessageAndAgency ps) => Show (TraceSendRecv ps) where
  show (TraceSendMsg msg) = "Send " ++ show msg
  show (TraceRecvMsg msg) = "Recv " ++ show msg


data DecoderFailure where
    DecoderFailure :: forall (pr :: PeerRole) ps (st :: ps) failure.
                      ( forall (st' :: ps). Show (ClientHasAgency st')
                      , forall (st' :: ps). Show (ServerHasAgency st')
                      , ShowProxy ps
                      , Show failure
                      )
                   => PeerHasAgency pr st
                   -> failure
                   -> DecoderFailure

instance Show DecoderFailure where
    show (DecoderFailure (tok :: PeerHasAgency pr (st :: ps)) failure) =
      concat
        [ "DecoderFailure ("
        , showProxy (Proxy :: Proxy ps)
        , ") "
        , show tok
        , ") ("
        , show failure
        , ")"
        ]

instance Exception DecoderFailure where


mkSimpleDriver :: forall ps failure bytes m f annotator.
                 ( MonadThrow m
                 , Show failure
                 , forall (st :: ps). Show (ClientHasAgency st)
                 , forall (st :: ps). Show (ServerHasAgency st)
                 , ShowProxy ps
                 )
              => (forall a.
                      Channel m bytes
                   -> Maybe bytes
                   -> DecodeStep bytes failure m (f a)
                   -> m (Either failure (a, Maybe bytes))
                 )
              -- ^ run incremental decoder against a channel

              -> (forall st. annotator st -> f (SomeMessage st))
              -- ^ transform annotator to a container holding the decoded
              -- message

              -> Tracer m (TraceSendRecv ps)
              -> CodecF ps failure m annotator bytes
              -> Channel m bytes
              -> Driver ps (Maybe bytes) m

mkSimpleDriver runDecodeSteps nat tracer Codec{encode, decode} channel@Channel{send} =
    Driver { sendMessage, recvMessage, startDState = Nothing }
  where
    sendMessage :: forall (pr :: PeerRole) (st :: ps) (st' :: ps).
                   PeerHasAgency pr st
                -> Message ps st st'
                -> m ()
    sendMessage stok msg = do
      send (encode stok msg)
      traceWith tracer (TraceSendMsg (AnyMessageAndAgency stok msg))

    recvMessage :: forall (pr :: PeerRole) (st :: ps).
                   PeerHasAgency pr st
                -> Maybe bytes
                -> m (SomeMessage st, Maybe bytes)
    recvMessage stok trailing = do
      decoder <- decode stok
      result  <- runDecodeSteps channel trailing (nat <$> decoder)
      case result of
        Right x@(SomeMessage msg, _trailing') -> do
          traceWith tracer (TraceRecvMsg (AnyMessageAndAgency stok msg))
          return x
        Left failure ->
          throwIO (DecoderFailure stok failure)


simpleDriver :: forall ps failure bytes m.
                ( MonadThrow m
                , Show failure
                , forall (st :: ps). Show (ClientHasAgency st)
                , forall (st :: ps). Show (ServerHasAgency st)
                , ShowProxy ps
                )
             => Tracer m (TraceSendRecv ps)
             -> Codec ps failure m bytes
             -> Channel m bytes
             -> Driver ps (Maybe bytes) m
simpleDriver = mkSimpleDriver runDecoderWithChannel Identity


annotatedSimpleDriver
             :: forall ps failure bytes m.
                ( MonadThrow m
                , Monoid bytes
                , Show failure
                , forall (st :: ps). Show (ClientHasAgency st)
                , forall (st :: ps). Show (ServerHasAgency st)
                , ShowProxy ps
                )
             => Tracer m (TraceSendRecv ps)
             -> AnnotatedCodec ps failure m bytes
             -> Channel m bytes
             -> Driver ps (Maybe bytes) m
annotatedSimpleDriver = mkSimpleDriver runAnnotatedDecoderWithChannel runAnnotator


-- | Run a peer with the given channel via the given codec.
--
-- This runs the peer to completion (if the protocol allows for termination).
--
runPeer
  :: forall ps (st :: ps) pr failure bytes m a .
     ( MonadThrow m
     , Show failure
     , forall (st' :: ps). Show (ClientHasAgency st')
     , forall (st' :: ps). Show (ServerHasAgency st')
     , ShowProxy ps
     )
  => Tracer m (TraceSendRecv ps)
  -> Codec ps failure m bytes
  -> Channel m bytes
  -> Peer ps pr st m a
  -> m (a, Maybe bytes)
runPeer tracer codec channel peer =
    runPeerWithDriver driver peer (startDState driver)
  where
    driver = simpleDriver tracer codec channel


-- | Run a peer with the given channel via the given annotated codec.
--
-- This runs the peer to completion (if the protocol allows for termination).
--
runAnnotatedPeer
  :: forall ps (st :: ps) pr failure bytes m a .
     ( MonadThrow m
     , Monoid bytes
     , Show failure
     , forall (st' :: ps). Show (ClientHasAgency st')
     , forall (st' :: ps). Show (ServerHasAgency st')
     , ShowProxy ps
     )
  => Tracer m (TraceSendRecv ps)
  -> AnnotatedCodec ps failure m bytes
  -> Channel m bytes
  -> Peer ps pr st m a
  -> m (a, Maybe bytes)
runAnnotatedPeer tracer codec channel peer =
    runPeerWithDriver driver peer (startDState driver)
  where
    driver = annotatedSimpleDriver tracer codec channel


-- | Run a pipelined peer with the given channel via the given codec.
--
-- This runs the peer to completion (if the protocol allows for termination).
--
-- Unlike normal peers, running pipelined peers rely on concurrency, hence the
-- 'MonadAsync' constraint.
--
runPipelinedPeer
  :: forall ps (st :: ps) pr failure bytes m a.
     ( MonadAsync m
     , MonadThrow m
     , Show failure
     , forall (st' :: ps). Show (ClientHasAgency st')
     , forall (st' :: ps). Show (ServerHasAgency st')
     , ShowProxy ps
     )
  => Tracer m (TraceSendRecv ps)
  -> Codec ps failure m bytes
  -> Channel m bytes
  -> PeerPipelined ps pr st m a
  -> m (a, Maybe bytes)
runPipelinedPeer tracer codec channel peer =
    runPipelinedPeerWithDriver driver peer (startDState driver)
  where
    driver = simpleDriver tracer codec channel


-- | Run a pipelined peer with the given channel via the given annotated codec.
--
-- This runs the peer to completion (if the protocol allows for termination).
--
-- Unlike normal peers, running pipelined peers rely on concurrency, hence the
-- 'MonadAsync' constraint.
--
runPipelinedAnnotatedPeer
  :: forall ps (st :: ps) pr failure bytes m a.
     ( MonadAsync m
     , MonadThrow m
     , Monoid bytes
     , Show failure
     , forall (st' :: ps). Show (ClientHasAgency st')
     , forall (st' :: ps). Show (ServerHasAgency st')
     , ShowProxy ps
     )
  => Tracer m (TraceSendRecv ps)
  -> AnnotatedCodec ps failure m bytes
  -> Channel m bytes
  -> PeerPipelined ps pr st m a
  -> m (a, Maybe bytes)
runPipelinedAnnotatedPeer tracer codec channel peer =
    runPipelinedPeerWithDriver driver peer (startDState driver)
  where
    driver = annotatedSimpleDriver tracer codec channel


--
-- Utils
--

-- | Run a codec incremental decoder 'DecodeStep' against a channel. It also
-- takes any extra input data and returns any unused trailing data.
--
runDecoderWithChannel :: Monad m
                      => Channel m bytes
                      -> Maybe bytes
                      -> DecodeStep bytes failure m (Identity a)
                      -> m (Either failure (a, Maybe bytes))

runDecoderWithChannel Channel{recv} = go
  where
    go _ (DecodeDone (Identity x) trailing) = return (Right (x, trailing))
    go _ (DecodeFail failure)               = return (Left failure)
    go Nothing (DecodePartial k) = recv >>= k >>= go Nothing
    go queued  (DecodePartial k) = k queued   >>= go Nothing


runAnnotatedDecoderWithChannel
  :: forall m bytes failure a.
     ( Monad m
     , Monoid bytes
     )
  => Channel m bytes
  -> Maybe bytes
  -> DecodeStep bytes failure m (bytes -> a)
  -> m (Either failure (a, Maybe bytes))

runAnnotatedDecoderWithChannel Channel{recv} = go <*> fromMaybe mempty
  where
    go _       consumed (DecodeDone f trailing) = return $ Right (f consumed, trailing)
    go _       _        (DecodeFail failure)    = return $ Left failure
    go Nothing consumed (DecodePartial k) = recv >>= \bs -> k bs >>= go Nothing (consumed <> fromMaybe mempty bs)
    go (Just queued) consumed (DecodePartial k) = k (Just queued)    >>= go Nothing (consumed <> queued)


data Role = Client | Server

-- | Run two 'Peer's via a pair of connected 'Channel's and a common 'Codec'.
--
-- This is useful for tests and quick experiments.
--
-- The first argument is expected to create two channels that are connected,
-- for example 'createConnectedChannels'.
--
runConnectedPeers :: ( MonadAsync m
                     , MonadCatch m
                     , Show failure
                     , forall (st' :: ps). Show (ClientHasAgency st')
                     , forall (st' :: ps). Show (ServerHasAgency st')
                     , ShowProxy ps
                     )
                  => m (Channel m bytes, Channel m bytes)
                  -> Tracer m (Role, TraceSendRecv ps)
                  -> Codec ps failure m bytes
                  -> Peer ps pr st m a
                  -> Peer ps (FlipAgency pr) st m b
                  -> m (a, b)
runConnectedPeers createChannels tracer codec client server =
    createChannels >>= \(clientChannel, serverChannel) ->

    (fst <$> runPeer tracerClient codec clientChannel client)
      `concurrently`
    (fst <$> runPeer tracerServer codec serverChannel server)
  where
    tracerClient = contramap ((,) Client) tracer
    tracerServer = contramap ((,) Server) tracer


-- | Run the same protocol with different codes.  This is useful for testing
-- 'Handshake' protocol which knows how to decode different versions.
--
runConnectedPeersAsymmetric
    :: ( MonadAsync m
       , MonadCatch m
       , Show failure
       , forall (st' :: ps). Show (ClientHasAgency st')
       , forall (st' :: ps). Show (ServerHasAgency st')
       , ShowProxy ps
       )
    => m (Channel m bytes, Channel m bytes)
    -> Tracer m (Role, TraceSendRecv ps)
    -> Codec ps failure m bytes
    -> Codec ps failure m bytes
    -> Peer ps pr st m a
    -> Peer ps (FlipAgency pr) st m b
    -> m (a, b)
runConnectedPeersAsymmetric createChannels tracer codec codec' client server =
    createChannels >>= \(clientChannel, serverChannel) ->

    (fst <$> runPeer tracerClient codec  clientChannel client)
      `concurrently`
    (fst <$> runPeer tracerServer codec' serverChannel server)
  where
    tracerClient = contramap ((,) Client) tracer
    tracerServer = contramap ((,) Server) tracer


runConnectedPeersPipelined :: ( MonadAsync m
                              , MonadCatch m
                              , Show failure
                              , forall (st' :: ps). Show (ClientHasAgency st')
                              , forall (st' :: ps). Show (ServerHasAgency st')
                              , ShowProxy ps
                              )
                           => m (Channel m bytes, Channel m bytes)
                           -> Tracer m (Role, TraceSendRecv ps)
                           -> Codec ps failure m bytes
                           -> PeerPipelined ps pr st m a
                           -> Peer          ps (FlipAgency pr) st m b
                           -> m (a, b)
runConnectedPeersPipelined createChannels tracer codec client server =
    createChannels >>= \(clientChannel, serverChannel) ->

    (fst <$> runPipelinedPeer tracerClient codec clientChannel client)
      `concurrently`
    (fst <$> runPeer          tracerServer codec serverChannel server)
  where
    tracerClient = contramap ((,) Client) tracer
    tracerServer = contramap ((,) Server) tracer
