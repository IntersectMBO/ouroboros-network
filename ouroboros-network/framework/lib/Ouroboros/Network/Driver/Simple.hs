{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
-- @UndecidableInstances@ extensions is required for defining @Show@ instance
-- of @'TraceSendRecv'@.
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

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
    -- * Util
  , runDecoderWithChannel
  , runDecoderWithChannel_DecodeDone
    -- * Connected peers
    -- TODO: move these to a test lib
  , Role (..)
  , runConnectedPeers
  , runAnnotatedConnectedPeers
  , runConnectedPeersAsymmetric
  , runConnectedPeersPipelined
  ) where

import Control.Applicative ((<|>))
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap

import Network.TypedProtocol.Codec
import Network.TypedProtocol.Core
import Network.TypedProtocol.Driver
import Network.TypedProtocol.Peer

import Ouroboros.Network.Channel
import Ouroboros.Network.Protocol.Limits (BearerBytes (bearerBytesSize))
import Ouroboros.Network.Util.ShowProxy

import Control.DeepSeq (NFData, force)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Tracer (Tracer (..), contramap, traceWith)
import Data.Functor.Identity (Identity (..))


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
-- 'TraceSendMsg' carries the time the @send@ happened.
-- 'TraceRecvMsg' carries the arrival time of the last byte of the message that
-- the decoder consumed; this may be 'Nothing' for 'Channel' implementations
-- that cannot provide arrival times.
--
data TraceSendRecv ps where
     TraceSendMsg :: Time -> AnyMessage ps -> TraceSendRecv ps
     TraceRecvMsg :: Maybe Time -> AnyMessage ps -> TraceSendRecv ps

instance Show (AnyMessage ps) => Show (TraceSendRecv ps) where
  show (TraceSendMsg tm msg) = "Send " ++ show tm ++ " " ++ show msg
  show (TraceRecvMsg mbTm msg) =
    let s = case mbTm of
          Nothing -> "Nothing"
          Just x  -> "(Just " ++ show x ++ ")"
    in "Recv " ++ s ++ " " ++ show msg


data DecoderFailure where
    DecoderFailure :: forall ps (st :: ps) failure.
                      ( Show failure
                      , Show (StateToken st)
                      , ShowProxy ps
                      , ActiveState st
                      )
                   => StateToken st
                   -> failure
                   -> DecoderFailure

instance Show DecoderFailure where
    show (DecoderFailure (tok :: StateToken (st :: ps)) failure) =
      concat
        [ "DecoderFailure ("
        , showProxy (Proxy :: Proxy ps)
        , ") "
        , show (activeAgency :: ActiveAgency st)
        , " ("
        , show tok
        , ") ("
        , show failure
        , ")"
        ]

instance Exception DecoderFailure where


mkSimpleDriver :: forall ps (pr :: PeerRole) failure bytes m f annotator.
                 ( MonadMonotonicTime m
                 , MonadThrow m
                 , ShowProxy ps
                 , forall (st :: ps) stok. stok ~ StateToken st => Show stok
                 , Show failure
                 )
              => (forall a.
                      Channel m bytes
                   -> Maybe (Reception bytes)
                   -> DecodeStep bytes failure m (f a)
                   -> m (Either failure (a, Maybe Time, Maybe (Reception bytes)))
                 )
              -- ^ run incremental decoder against a channel; returns the
              -- arrival time of the last byte of the decoded message (when
              -- available) and the (possibly partial) next-message bytes
              -- with their per-byte arrival times.

              -> (forall (st :: ps). annotator st -> f (SomeMessage st))
              -- ^ transform annotator to a container holding the decoded
              -- message

              -> Tracer m (TraceSendRecv ps)
              -> CodecF ps failure m annotator bytes
              -> Channel m bytes
              -> Driver ps pr (Maybe (Reception bytes)) m

mkSimpleDriver runDecodeSteps nat tracer Codec{encode, decode} channel@Channel{send} =
    Driver { sendMessage, recvMessage, initialDState = Nothing }
  where
    sendMessage :: forall (st :: ps) (st' :: ps).
                   StateTokenI st
                => ActiveState st
                => WeHaveAgencyProof pr st
                -> Message ps st st'
                -> m ()
    sendMessage !_ msg = do
      tm <- getMonotonicTime
      send (encode msg)
      traceWith tracer (TraceSendMsg tm (AnyMessage msg))

    recvMessage :: forall (st :: ps).
                   StateTokenI st
                => ActiveState st
                => TheyHaveAgencyProof pr st
                -> Maybe (Reception bytes)
                -> m (SomeMessage st, Maybe (Reception bytes))
    recvMessage !_ trailing = do
      let tok = stateToken
      decoder <- decode tok
      result  <- runDecodeSteps channel trailing (nat <$> decoder)
      case result of
        Right (SomeMessage !msg, mbTm, trailing') -> do
          traceWith tracer (TraceRecvMsg mbTm (AnyMessage msg))
          return (SomeMessage msg, trailing')
        Left failure ->
          throwIO (DecoderFailure tok failure)


simpleDriver :: forall ps (pr :: PeerRole) failure bytes m.
                ( MonadEvaluate m
                , MonadMonotonicTime m
                , MonadThrow m
                , ShowProxy ps
                , forall (st :: ps) stok. stok ~ StateToken st => Show stok
                , NFData failure
                , Show failure
                , BearerBytes bytes
                )
             => Tracer m (TraceSendRecv ps)
             -> Codec ps failure m bytes
             -> Channel m bytes
             -> Driver ps pr (Maybe (Reception bytes)) m
simpleDriver = mkSimpleDriver runDecoderWithChannel Identity


annotatedSimpleDriver
             :: forall ps (pr :: PeerRole) failure bytes m.
                ( MonadEvaluate m
                , MonadMonotonicTime m
                , MonadThrow m
                , Monoid bytes
                , ShowProxy ps
                , forall (st :: ps) stok. stok ~ StateToken st => Show stok
                , NFData failure
                , Show failure
                , BearerBytes bytes
                )
             => Tracer m (TraceSendRecv ps)
             -> AnnotatedCodec ps failure m bytes
             -> Channel m bytes
             -> Driver ps pr (Maybe (Reception bytes)) m
annotatedSimpleDriver = mkSimpleDriver runAnnotatedDecoderWithChannel runAnnotator


-- | Run a peer with the given channel via the given codec.
--
-- This runs the peer to completion (if the protocol allows for termination).
--
runPeer
  :: forall ps (st :: ps) pr failure bytes m a .
     ( MonadEvaluate m
     , MonadMonotonicTime m
     , MonadThrow m
     , ShowProxy ps
     , forall (st' :: ps) stok. stok ~ StateToken st' => Show stok
     , NFData a
     , NFData failure
     , Show failure
     , BearerBytes bytes
     )
  => Tracer m (TraceSendRecv ps)
  -> Codec ps failure m bytes
  -> Channel m bytes
  -> Peer ps pr NonPipelined st m a
  -> m (a, Maybe (Reception bytes))
runPeer tracer codec channel peer =
    runPeerWithDriver driver peer
  where
    driver = simpleDriver tracer codec channel


-- | Run a peer with the given channel via the given annotated codec.
--
-- This runs the peer to completion (if the protocol allows for termination).
--
runAnnotatedPeer
  :: forall ps (st :: ps) pr failure bytes m a .
     ( MonadEvaluate m
     , MonadMonotonicTime m
     , MonadThrow m
     , Monoid bytes
     , ShowProxy ps
     , forall (st' :: ps) stok. stok ~ StateToken st' => Show stok
     , NFData a
     , NFData failure
     , Show failure
     , BearerBytes bytes
     )
  => Tracer m (TraceSendRecv ps)
  -> AnnotatedCodec ps failure m bytes
  -> Channel m bytes
  -> Peer ps pr NonPipelined st m a
  -> m (a, Maybe (Reception bytes))
runAnnotatedPeer tracer codec channel peer =
    runPeerWithDriver driver peer
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
     , MonadEvaluate m
     , MonadMonotonicTime m
     , MonadThrow m
     , ShowProxy ps
     , forall (st' :: ps) stok. stok ~ StateToken st' => Show stok
     , NFData a
     , NFData failure
     , Show failure
     , BearerBytes bytes
     )
  => Tracer m (TraceSendRecv ps)
  -> Codec ps failure m bytes
  -> Channel m bytes
  -> PeerPipelined ps pr st m a
  -> m (a, Maybe (Reception bytes))
runPipelinedPeer tracer codec channel peer =
    runPipelinedPeerWithDriver driver peer
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
     , MonadEvaluate m
     , MonadMonotonicTime m
     , MonadThrow m
     , Monoid bytes
     , ShowProxy ps
     , forall (st' :: ps) stok. stok ~ StateToken st' => Show stok
     , NFData a
     , NFData failure
     , Show failure
     , BearerBytes bytes
     )
  => Tracer m (TraceSendRecv ps)
  -> AnnotatedCodec ps failure m bytes
  -> Channel m bytes
  -> PeerPipelined ps pr st m a
  -> m (a, Maybe (Reception bytes))
runPipelinedAnnotatedPeer tracer codec channel peer =
    runPipelinedPeerWithDriver driver peer
  where
    driver = annotatedSimpleDriver tracer codec channel

--
-- Utils
--

-- | Given the per-byte arrival-time map of a single 'Reception', the total
-- 'Reception' byte size and how many of its trailing bytes were left unconsumed
-- by the decoder, compute:
--
-- * 'Just' the arrival time of the last byte the decoder consumed (or
--   'Nothing' if no arrival time was provided for any consumed byte), and
-- * the rekeyed arrival-time map for the unconsumed trailing bytes, with the
--   message-completion time inserted at offset 0 (so the next decode's
--   first-byte time isn't lost if the message boundary fell strictly inside a
--   chunk).
--
runDecoderWithChannel_DecodeDone :: IntMap Time -> Int -> Int -> (Maybe Time, IntMap Time)
runDecoderWithChannel_DecodeDone tms rsz nTrailing =
    let nConsumed = rsz - nTrailing
        (lt, mbEq, gt) = IntMap.splitLookup nConsumed tms
        mbTm :: Maybe Time
        mbTm = case IntMap.lookupMax lt of
            Nothing      -> Nothing  -- this 'Channel' did not provide a 'Time' for this byte
            Just (_, tm) -> Just tm

        gt' = IntMap.mapKeysMonotonic (\key -> key - nConsumed) gt
        tms' :: IntMap Time
        tms' = case mbEq <|> mbTm of
            Nothing -> gt'
            Just tm -> IntMap.insert 0 tm gt'
    in
    (mbTm, tms')

-- | Run a codec incremental decoder 'DecodeStep' against a channel. It also
-- takes any extra input data and returns any unused trailing data, alongside
-- the arrival time of the last byte the decoder consumed.
--
runDecoderWithChannel :: forall m bytes failure a.
                         ( Monad m
                         , MonadEvaluate m
                         , NFData failure
                         , BearerBytes bytes
                         )
                      => Channel m bytes
                      -> Maybe (Reception bytes)
                      -> DecodeStep bytes failure m (Identity a)
                      -> m (Either failure (a, Maybe Time, Maybe (Reception bytes)))

runDecoderWithChannel Channel{recv} =
    \trailing step -> go IntMap.empty 0 trailing step
  where
    size = bearerBytesSize

    go :: IntMap Time
       -> Word
       -> Maybe (Reception bytes)
       -> DecodeStep bytes failure m (Identity a)
       -> m (Either failure (a, Maybe Time, Maybe (Reception bytes)))
    go tms rsz _ (DecodeDone (Identity x) trailing) =
      let (mbTm, tms') = runDecoderWithChannel_DecodeDone
                            tms
                            (fromIntegral rsz)
                            (fromIntegral $ maybe 0 size trailing)
      in
      return (Right (x, mbTm, MkReception tms' <$> trailing))
    go _ _ _ (DecodeFail failure) = Left <$> evaluate (force failure)
    go _ _ Nothing (DecodePartial k) = do
      mRcptn <- recv
      let (!tms', !rsz', mbs) = case mRcptn of
            Nothing                   -> (IntMap.empty, 0, Nothing)
            Just (MkReception tms bs) -> (tms, size bs, Just bs)
      k mbs >>= go tms' rsz' Nothing
    go _ _ (Just (MkReception tms trailing)) (DecodePartial k) =
      k (Just trailing) >>= go tms (size trailing) Nothing


runAnnotatedDecoderWithChannel
  :: forall m bytes failure a.
     ( Monad m
     , MonadEvaluate m
     , Monoid bytes
     , NFData failure
     , BearerBytes bytes
     )
  => Channel m bytes
  -> Maybe (Reception bytes)
  -> DecodeStep bytes failure m (bytes -> a)
  -> m (Either failure (a, Maybe Time, Maybe (Reception bytes)))

runAnnotatedDecoderWithChannel Channel{recv} =
    \trailing step -> go [] IntMap.empty 0 trailing step
  where
    size = bearerBytesSize

    go :: [bytes]
       -> IntMap Time
       -> Int            -- ^ size of the latest 'Reception'
       -> Maybe (Reception bytes)
       -> DecodeStep bytes failure m (bytes -> a)
       -> m (Either failure (a, Maybe Time, Maybe (Reception bytes)))
    go !bytes _ _ (Just (MkReception tms' trailing)) (DecodePartial k) =
      -- INVARIANT only reachable on the first invocation; @tms@ and @rsz@ are
      -- empty.
      k (Just trailing) >>= go (trailing : bytes) tms' (fromIntegral $ size trailing) Nothing
    go !bytes _ _ Nothing (DecodePartial k) = do
      mRcptn <- recv
      let (!tms', !rsz', mbs) = case mRcptn of
            Nothing                   -> (IntMap.empty, 0, Nothing)
            Just (MkReception tms bs) -> (tms, size bs, Just bs)
      k mbs >>= go (maybe bytes (: bytes) mbs) tms' (fromIntegral rsz') Nothing
    go !bytes tms rsz _ (DecodeDone f trailing) =
      let (mbTm, tms') = runDecoderWithChannel_DecodeDone
                            tms
                            rsz
                            (fromIntegral $ maybe 0 size trailing)
      in
      return $ Right (f $ mconcat (reverse bytes), mbTm, MkReception tms' <$> trailing)
    go _bytes _ _ _ (DecodeFail failure) = Left <$> evaluate (force failure)


data Role = Client | Server
  deriving Show

-- | Run two 'Peer's via a pair of connected 'Channel's and a common 'Codec'.
--
-- This is useful for tests and quick experiments.
--
-- The first argument is expected to create two channels that are connected,
-- for example 'createConnectedChannels'.
--
runConnectedPeers :: forall ps pr st failure bytes m a b.
                     ( MonadAsync m
                     , MonadEvaluate m
                     , MonadMonotonicTime m
                     , MonadThrow m
                     , ShowProxy ps
                     , forall (st' :: ps) stok. stok ~ StateToken st' => Show stok
                     , NFData a
                     , NFData b
                     , NFData failure
                     , Show failure
                     , BearerBytes bytes
                     )
                  => m (Channel m bytes, Channel m bytes)
                  -> Tracer m (Role, TraceSendRecv ps)
                  -> Codec ps failure m bytes
                  -> Peer ps             pr  NonPipelined st m a
                  -> Peer ps (FlipAgency pr) NonPipelined st m b
                  -> m (a, b)
runConnectedPeers createChannels tracer codec client server =
    createChannels >>= \(clientChannel, serverChannel) ->

    (do labelThisThread "client"
        fst <$> runPeer tracerClient codec clientChannel client
    )
      `concurrently`
    (do labelThisThread "server"
        fst <$> runPeer tracerServer codec serverChannel server
    )
  where
    tracerClient = contramap ((,) Client) tracer
    tracerServer = contramap ((,) Server) tracer


runAnnotatedConnectedPeers
  :: forall ps pr st failure bytes m a b.
     ( MonadAsync m
     , MonadEvaluate m
     , MonadMonotonicTime m
     , MonadThrow m
     , ShowProxy ps
     , forall (st' :: ps) stok. stok ~ StateToken st' => Show stok
     , NFData a
     , NFData b
     , NFData failure
     , Show failure
     , Monoid bytes
     , BearerBytes bytes
     )
  => m (Channel m bytes, Channel m bytes)
  -> Tracer m (Role, TraceSendRecv ps)
  -> AnnotatedCodec ps failure m bytes
  -> Peer ps             pr  NonPipelined st m a
  -> Peer ps (FlipAgency pr) NonPipelined st m b
  -> m (a, b)
runAnnotatedConnectedPeers createChannels tracer codec client server =
    createChannels >>= \(clientChannel, serverChannel) ->

    (do labelThisThread "client"
        fst <$> runAnnotatedPeer tracerClient codec clientChannel client
    )
      `concurrently`
    (do labelThisThread "server"
        fst <$> runAnnotatedPeer tracerServer codec serverChannel server
    )
  where
    tracerClient = contramap ((,) Client) tracer
    tracerServer = contramap ((,) Server) tracer


runConnectedPeersPipelined :: ( MonadAsync m
                              , MonadCatch m
                              , MonadEvaluate m
                              , MonadMonotonicTime m
                              , ShowProxy ps
                              , forall (st' :: ps) stok. stok ~ StateToken st' => Show stok
                              , NFData a
                              , NFData b
                              , NFData failure
                              , Show failure
                              , BearerBytes bytes
                              )
                           => m (Channel m bytes, Channel m bytes)
                           -> Tracer m (Role, TraceSendRecv ps)
                           -> Codec ps failure m bytes
                           -> PeerPipelined ps             pr               st m a
                           -> Peer          ps (FlipAgency pr) NonPipelined st m b
                           -> m (a, b)
runConnectedPeersPipelined createChannels tracer codec client server =
    createChannels >>= \(clientChannel, serverChannel) ->

    (fst <$> runPipelinedPeer tracerClient codec clientChannel client)
      `concurrently`
    (fst <$> runPeer          tracerServer codec serverChannel server)
  where
    tracerClient = contramap ((,) Client) tracer
    tracerServer = contramap ((,) Server) tracer


-- | Run the same protocol with different codecs.  This is useful for testing
-- 'Handshake' protocol which knows how to decode different versions.
--
runConnectedPeersAsymmetric
    :: ( MonadAsync m
       , MonadEvaluate m
       , MonadMask  m
       , MonadMonotonicTime m
       , ShowProxy ps
       , forall (st' :: ps) stok. stok ~ StateToken st' => Show stok
       , NFData a
       , NFData b
       , NFData failure
       , Show failure
       , BearerBytes bytes
       )
    => m (Channel m bytes, Channel m bytes)
    -> Tracer m (Role, TraceSendRecv ps)
    -> Codec ps failure m bytes
    -> Codec ps failure m bytes
    -> Peer ps             pr  NonPipelined st m a
    -> Peer ps (FlipAgency pr) NonPipelined st m b
    -> m (a, b)
runConnectedPeersAsymmetric createChannels tracer codec codec' client server =
    createChannels >>= \(clientChannel, serverChannel) ->

    (fst <$> runPeer tracerClient codec  clientChannel client)
      `concurrently`
    (fst <$> runPeer tracerServer codec' serverChannel server)
  where
    tracerClient = contramap ((,) Client) tracer
    tracerServer = contramap ((,) Server) tracer
