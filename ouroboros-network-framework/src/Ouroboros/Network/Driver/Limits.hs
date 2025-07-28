{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Drivers for running 'Peer's.
--
module Ouroboros.Network.Driver.Limits
  ( -- * Limits
    ProtocolSizeLimits (..)
  , ProtocolTimeLimits (..)
  , ProtocolTimeLimitsWithRnd (..)
  , ProtocolLimitFailure (..)
    -- * Normal peers
  , runPeerWithLimits
  , runPipelinedPeerWithLimits
  , runPeerWithLimitsRnd
  , runPipelinedPeerWithLimitsRnd
  , runAnnotatedPeerWithLimits
  , runPipelinedAnnotatedPeerWithLimits
    -- TODO:
    -- runAnnotatedPeerWithLimitsRnd
    -- runPipelinedAnnotatedPeerWithLimitsRnd
  , TraceSendRecv (..)
    -- * Driver utilities
  , driverWithLimits
  , driverWithLimitsRnd
  , annotatedDriverWithLimits
    -- TODO:
    -- annotedDriverWithLimitsRnd
  , runConnectedPeersWithLimits
  , runConnectedPipelinedPeersWithLimits
  , runConnectedPeersWithLimitsRnd
  , runConnectedPipelinedPeersWithLimitsRnd
  ) where

import Data.Bifunctor (first)
import Data.Functor.Identity
import Data.Maybe (fromMaybe)
import System.Random

import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer (Tracer (..), contramap, traceWith)

import Network.Mux.Timeout
import Network.TypedProtocol.Codec
import Network.TypedProtocol.Core
import Network.TypedProtocol.Driver
import Network.TypedProtocol.Peer

import Ouroboros.Network.Channel
import Ouroboros.Network.Driver.Simple
import Ouroboros.Network.Protocol.Limits
import Ouroboros.Network.Util.ShowProxy


mkDriverWithLimits
  :: forall ps (pr :: PeerRole) failure bytes m f annotator.
     ( MonadThrow m
     , ShowProxy ps
     , forall (st' :: ps) tok. tok ~ StateToken st' => Show tok
     , Show failure
     )
  => (forall a.
           Word
        -> (bytes -> Word)
        -> Channel m bytes
        -> Maybe bytes
        -> DecodeStep bytes failure m (f a)
        -> m (Either (Maybe failure) (a, Maybe bytes))
     )
  -- ^ run incremental decoder against a channel
  --

  -> (forall (st :: ps). annotator st -> f (SomeMessage st))
  -- ^ transform annotator to a container holding the decoded
  -- message

  -> Tracer m (TraceSendRecv ps)
  -> TimeoutFn m
  -> CodecF ps failure m annotator bytes
  -> ProtocolSizeLimits ps bytes
  -> ProtocolTimeLimits ps
  -> Channel m bytes
  -> Driver ps pr (Maybe bytes) m
mkDriverWithLimits runDecodeStep nat tracer
                   timeoutFn
                   Codec{encode, decode}
                   ProtocolSizeLimits{sizeLimitForState, dataSize}
                   ProtocolTimeLimits{timeLimitForState}
                   channel@Channel{send} =
    Driver { sendMessage, recvMessage, initialDState = Nothing }
  where
    sendMessage :: forall (st :: ps) (st' :: ps).
                   StateTokenI st
                => ActiveState st
                => WeHaveAgencyProof pr st
                -> Message ps st st'
                -> m ()
    sendMessage !_ msg = do
      send (encode msg)
      traceWith tracer (TraceSendMsg (AnyMessage msg))


    recvMessage :: forall (st :: ps).
                   StateTokenI st
                => ActiveState st
                => TheyHaveAgencyProof pr st
                -> Maybe bytes
                -> m (SomeMessage st, Maybe bytes)
    recvMessage !_ trailing = do
      let tok = stateToken
      decoder <- decode tok
      let sizeLimit = sizeLimitForState @st stateToken
          timeLimit = fromMaybe (-1) (timeLimitForState @st stateToken)
      result  <- timeoutFn timeLimit $
                   runDecodeStep sizeLimit dataSize
                                 channel trailing (nat <$> decoder)

      case result of
        Just (Right x@(SomeMessage msg, _trailing')) -> do
          traceWith tracer (TraceRecvMsg (AnyMessage msg))
          return x
        Just (Left (Just failure)) -> throwIO (DecoderFailure tok failure)
        Just (Left Nothing)        -> throwIO (ExceededSizeLimit tok)
        Nothing                    -> throwIO (ExceededTimeLimit tok)


driverWithLimits
  :: forall ps (pr :: PeerRole) failure bytes m.
     ( MonadThrow m
     , ShowProxy ps
     , forall (st' :: ps) tok. tok ~ StateToken st' => Show tok
     , Show failure
     )
  => Tracer m (TraceSendRecv ps)
  -> TimeoutFn m
  -> Codec ps failure m bytes
  -> ProtocolSizeLimits ps bytes
  -> ProtocolTimeLimits ps
  -> Channel m bytes
  -> Driver ps pr (Maybe bytes) m
driverWithLimits = mkDriverWithLimits runDecoderWithLimit Identity


annotatedDriverWithLimits
  :: forall ps (pr :: PeerRole) failure bytes m.
     ( MonadThrow m
     , Monoid bytes
     , ShowProxy ps
     , forall (st' :: ps) tok. tok ~ StateToken st' => Show tok
     , Show failure
     )
  => Tracer m (TraceSendRecv ps)
  -> TimeoutFn m
  -> AnnotatedCodec ps failure m bytes
  -> ProtocolSizeLimits ps bytes
  -> ProtocolTimeLimits ps
  -> Channel m bytes
  -> Driver ps pr (Maybe bytes) m
annotatedDriverWithLimits = mkDriverWithLimits runAnnotatedDecoderWithLimit runAnnotator


mkDriverWithLimitsRnd
  :: forall ps (pr :: PeerRole) failure bytes m f annotator.
     ( MonadThrow m
     , ShowProxy ps
     , forall (st' :: ps) tok. tok ~ StateToken st' => Show tok
     , Show failure
     )
  => (forall a.
           Word
        -> (bytes -> Word)
        -> Channel m bytes
        -> Maybe bytes
        -> DecodeStep bytes failure m (f a)
        -> m (Either (Maybe failure) (a, Maybe bytes))
     )
  -- ^ run incremental decoder against a channel
  --

  -> (forall (st :: ps). annotator st -> f (SomeMessage st))
  -- ^ transform annotator to a container holding the decoded
  -- message

  -> Tracer m (TraceSendRecv ps)
  -> TimeoutFn m
  -> StdGen
  -> CodecF ps failure m annotator bytes
  -> ProtocolSizeLimits ps bytes
  -> ProtocolTimeLimitsWithRnd ps
  -> Channel m bytes
  -> Driver ps pr (Maybe bytes, StdGen) m
mkDriverWithLimitsRnd runDecodeStep nat tracer timeoutFn rnd0
                      Codec{encode, decode}
                      ProtocolSizeLimits{sizeLimitForState, dataSize}
                      ProtocolTimeLimitsWithRnd{timeLimitForStateWithRnd}
                      channel@Channel{send} =
    Driver { sendMessage, recvMessage, initialDState = (Nothing, rnd0) }
  where
    sendMessage :: forall (st :: ps) (st' :: ps).
                   StateTokenI st
                => ActiveState st
                => WeHaveAgencyProof pr st
                -> Message ps st st'
                -> m ()
    sendMessage !_ msg = do
      send (encode msg)
      traceWith tracer (TraceSendMsg (AnyMessage msg))


    recvMessage :: forall (st :: ps).
                   StateTokenI st
                => ActiveState st
                => TheyHaveAgencyProof pr st
                -> (Maybe bytes, StdGen)
                -> m (SomeMessage st, (Maybe bytes, StdGen))
    recvMessage !_ (trailing, !rnd) = do
      let tok = stateToken
      decoder <- decode tok
      let sizeLimit = sizeLimitForState @st stateToken
          (timeLimit, rnd') = first (fromMaybe (-1))
                            $ timeLimitForStateWithRnd @st stateToken rnd
      result  <- timeoutFn timeLimit $
                   runDecodeStep sizeLimit dataSize
                                 channel trailing (nat <$> decoder)

      case result of
        Just (Right (x@(SomeMessage msg), trailing')) -> do
          traceWith tracer (TraceRecvMsg (AnyMessage msg))
          return (x, (trailing', rnd'))
        Just (Left (Just failure)) -> throwIO (DecoderFailure tok failure)
        Just (Left Nothing)        -> throwIO (ExceededSizeLimit tok)
        Nothing                    -> throwIO (ExceededTimeLimit tok)


-- | Like 'driverWithLimits' but gives access to `StdGen` to generate
-- `ProtocolTimeouts` for the next state.
--
driverWithLimitsRnd :: forall ps (pr :: PeerRole) failure bytes m.
                       ( MonadThrow m
                       , ShowProxy ps
                       , forall (st' :: ps) tok. tok ~ StateToken st' => Show tok
                       , Show failure
                       )
                    => Tracer m (TraceSendRecv ps)
                    -> TimeoutFn m
                    -> StdGen
                    -> Codec ps failure m bytes
                    -> ProtocolSizeLimits ps bytes
                    -> ProtocolTimeLimitsWithRnd ps
                    -> Channel m bytes
                    -> Driver ps pr (Maybe bytes, StdGen) m
driverWithLimitsRnd = mkDriverWithLimitsRnd runDecoderWithLimit Identity


runDecoderWithLimit
    :: forall m bytes failure a. Monad m
    => Word
    -- ^ message size limit
    -> (bytes -> Word)
    -- ^ byte size
    -> Channel m bytes
    -> Maybe bytes
    -> DecodeStep bytes failure m (Identity a)
    -> m (Either (Maybe failure) (a, Maybe bytes))
runDecoderWithLimit limit size Channel{recv} =
    go 0
  where
    -- Our strategy here is as follows...
    --
    -- We of course want to enforce the maximum data limit, but we also want to
    -- detect and report when we exceed the limit rather than having it be
    -- misclassified as a generic decode error. For example if we simply limited
    -- the decoder input to the maximum size then the failure would be reported
    -- as an unexpected end of input, rather than that the size limit was
    -- exceeded.
    --
    -- So our strategy is to allow the last chunk of input to exceed the limit.
    -- This leaves just one special case: if the decoder finishes with that
    -- final chunk, we must check if it consumed too much of the final chunk.
    --
    go :: Word        -- ^ size of consumed input so far
       -> Maybe bytes -- ^ any trailing data
       -> DecodeStep bytes failure m (Identity a)
       -> m (Either (Maybe failure) (a, Maybe bytes))

    go !sz !_ (DecodeDone (Identity x) trailing)
      | let sz' = sz - maybe 0 size trailing
      , sz' > limit = return (Left Nothing)
      | otherwise   = return (Right (x, trailing))

    go !_ !_ (DecodeFail failure) = return (Left (Just failure))

    go !sz trailing (DecodePartial k)
      | sz > limit = return (Left Nothing)
      | otherwise  = case trailing of
                       Nothing -> do mbs <- recv
                                     let !sz' = sz + maybe 0 size mbs
                                     go sz' Nothing =<< k mbs
                       Just bs -> do let sz' = sz + size bs
                                     go sz' Nothing =<< k (Just bs)


runAnnotatedDecoderWithLimit
    :: forall m bytes failure a.
       ( Monad m
       , Monoid bytes
       )
    => Word
    -- ^ message size limit
    -> (bytes -> Word)
    -- ^ byte size
    -> Channel m bytes
    -> Maybe bytes
    -> DecodeStep bytes failure m (bytes -> a)
    -> m (Either (Maybe failure) (a, Maybe bytes))
runAnnotatedDecoderWithLimit limit size Channel{recv} =
    go mempty 0
  where
    -- Our strategy here is as follows...
    --
    -- We of course want to enforce the maximum data limit, but we also want to
    -- detect and report when we exceed the limit rather than having it be
    -- misclassified as a generic decode error. For example if we simply limited
    -- the decoder input to the maximum size then the failure would be reported
    -- as an unexpected end of input, rather than that the size limit was
    -- exceeded.
    --
    -- So our strategy is to allow the last chunk of input to exceed the limit.
    -- This leaves just one special case: if the decoder finishes with that
    -- final chunk, we must check if it consumed too much of the final chunk.
    --
    go :: [bytes]     -- ^ accumulation of chunks received from the network
       -> Word        -- ^ size of consumed input so far
       -> Maybe bytes -- ^ any trailing data
       -> DecodeStep bytes failure m (bytes -> a)
       -> m (Either (Maybe failure) (a, Maybe bytes))

    go !bytes !sz !_ (DecodeDone f trailing)
      | let sz' = sz - maybe 0 size trailing
      , sz' > limit = return (Left Nothing)
      | otherwise   = return (Right (f (mconcat $ reverse bytes), trailing))

    go !_ !_ !_ (DecodeFail failure) = return (Left (Just failure))

    go !_ !sz !_ DecodePartial {} | sz > limit =
      return (Left Nothing)

    go !bytes !sz Nothing (DecodePartial k) = do
      mbs <- recv
      let !sz' = sz + maybe 0 size mbs
      step <- k mbs
      go (case mbs of
            Nothing -> bytes
            Just bs -> bs : bytes)
         sz' Nothing step

    go !bytes !sz (Just trailing) (DecodePartial k) = do
      let sz' = sz + size trailing
      step <- k (Just trailing)
      go (trailing : bytes) sz' Nothing step


-- | Run a peer with limits.
--
runPeerWithLimits
  :: forall ps (st :: ps) pr failure bytes m a .
     ( MonadAsync m
     , MonadFork m
     , MonadMask m
     , MonadThrow (STM m)
     , MonadTimer m
     , ShowProxy ps
     , forall (st' :: ps) stok. stok ~ StateToken st' => Show stok
     , Show failure
     )
  => Tracer m (TraceSendRecv ps)
  -> Codec ps failure m bytes
  -> ProtocolSizeLimits ps bytes
  -> ProtocolTimeLimits ps
  -> Channel m bytes
  -> Peer ps pr NonPipelined st m a
  -> m (a, Maybe bytes)
runPeerWithLimits tracer codec slimits tlimits channel peer =
    withTimeoutSerial $ \timeoutFn ->
      let driver = driverWithLimits tracer timeoutFn codec slimits tlimits channel
      in runPeerWithDriver driver peer


-- | Run an peer with annotated codec & limits.
--
runAnnotatedPeerWithLimits
  :: forall ps (st :: ps) pr failure bytes m a .
     ( MonadAsync m
     , MonadFork m
     , MonadMask m
     , MonadThrow (STM m)
     , MonadTimer m
     , Monoid bytes
     , ShowProxy ps
     , forall (st' :: ps) stok. stok ~ StateToken st' => Show stok
     , Show failure
     )
  => Tracer m (TraceSendRecv ps)
  -> AnnotatedCodec ps failure m bytes
  -> ProtocolSizeLimits ps bytes
  -> ProtocolTimeLimits ps
  -> Channel m bytes
  -> Peer ps pr NonPipelined st m a
  -> m (a, Maybe bytes)
runAnnotatedPeerWithLimits tracer codec slimits tlimits channel peer =
    withTimeoutSerial $ \timeoutFn ->
      let driver = annotatedDriverWithLimits tracer timeoutFn codec slimits tlimits channel
      in runPeerWithDriver driver peer


-- | Run a peer with limits.  'ProtocolTimeLimits' have access to
-- a pseudorandom generator.
--
runPeerWithLimitsRnd
  :: forall ps (st :: ps) pr failure bytes m a .
     ( MonadAsync m
     , MonadFork m
     , MonadMask m
     , MonadThrow (STM m)
     , MonadTimer m
     , ShowProxy ps
     , forall (st' :: ps) stok. stok ~ StateToken st' => Show stok
     , Show failure
     )
  => Tracer m (TraceSendRecv ps)
  -> StdGen
  -> Codec ps failure m bytes
  -> ProtocolSizeLimits ps bytes
  -> ProtocolTimeLimitsWithRnd ps
  -> Channel m bytes
  -> Peer ps pr NonPipelined st m a
  -> m (a, Maybe bytes)
runPeerWithLimitsRnd tracer rnd codec slimits tlimits channel peer =
    withTimeoutSerial $ \timeoutFn ->
      let driver = driverWithLimitsRnd tracer timeoutFn rnd codec slimits tlimits channel
      in     (\(a, (trailing, _)) -> (a, trailing))
         <$> runPeerWithDriver driver peer


-- | Run a pipelined peer with the given channel via the given codec.
--
-- This runs the peer to completion (if the protocol allows for termination).
--
-- Unlike normal peers, running pipelined peers rely on concurrency, hence the
-- 'MonadAsync' constraint.
--
runPipelinedPeerWithLimits
  :: forall ps (st :: ps) pr failure bytes m a.
     ( MonadAsync m
     , MonadFork m
     , MonadMask m
     , MonadTimer m
     , MonadThrow (STM m)
     , ShowProxy ps
     , forall (st' :: ps) stok. stok ~ StateToken st' => Show stok
     , Show failure
     )
  => Tracer m (TraceSendRecv ps)
  -> Codec ps failure m bytes
  -> ProtocolSizeLimits ps bytes
  -> ProtocolTimeLimits ps
  -> Channel m bytes
  -> PeerPipelined ps pr st m a
  -> m (a, Maybe bytes)
runPipelinedPeerWithLimits tracer codec slimits tlimits channel peer =
    withTimeoutSerial $ \timeoutFn ->
      let driver = driverWithLimits tracer timeoutFn codec slimits tlimits channel
      in runPipelinedPeerWithDriver driver peer


runPipelinedAnnotatedPeerWithLimits
  :: forall ps (st :: ps) pr failure bytes m a.
     ( MonadAsync m
     , MonadFork m
     , MonadMask m
     , MonadTimer m
     , MonadThrow (STM m)
     , Monoid bytes
     , ShowProxy ps
     , forall (st' :: ps) stok. stok ~ StateToken st' => Show stok
     , Show failure
     )
  => Tracer m (TraceSendRecv ps)
  -> AnnotatedCodec ps failure m bytes
  -> ProtocolSizeLimits ps bytes
  -> ProtocolTimeLimits ps
  -> Channel m bytes
  -> PeerPipelined ps pr st m a
  -> m (a, Maybe bytes)
runPipelinedAnnotatedPeerWithLimits tracer codec slimits tlimits channel peer =
    withTimeoutSerial $ \timeoutFn ->
      let driver = annotatedDriverWithLimits tracer timeoutFn codec slimits tlimits channel
      in runPipelinedPeerWithDriver driver peer


-- | Like 'runPipelinedPeerWithLimits' but time limits have access to
-- a pseudorandom generator.
--
runPipelinedPeerWithLimitsRnd
  :: forall ps (st :: ps) pr failure bytes m a.
     ( MonadAsync m
     , MonadFork m
     , MonadMask m
     , MonadTimer m
     , MonadThrow (STM m)
     , ShowProxy ps
     , forall (st' :: ps) stok. stok ~ StateToken st' => Show stok
     , Show failure
     )
  => Tracer m (TraceSendRecv ps)
  -> StdGen
  -> Codec ps failure m bytes
  -> ProtocolSizeLimits ps bytes
  -> ProtocolTimeLimitsWithRnd ps
  -> Channel m bytes
  -> PeerPipelined ps pr st m a
  -> m (a, Maybe bytes)
runPipelinedPeerWithLimitsRnd tracer rnd codec slimits tlimits channel peer =
    withTimeoutSerial $ \timeoutFn ->
      let driver = driverWithLimitsRnd tracer timeoutFn rnd codec slimits tlimits channel
      in     (\(a, (trailing, _)) -> (a, trailing))
         <$> runPipelinedPeerWithDriver driver peer


-- | Run two 'Peer's via a pair of connected 'Channel's and a common 'Codec'.
-- The client side is using 'driverWithLimits'.
--
-- This is useful for tests and quick experiments.
--
-- The first argument is expected to create two channels that are connected,
-- for example 'createConnectedChannels'.
--
runConnectedPeersWithLimits
  :: forall ps pr st failure bytes m a b.
     ( MonadAsync       m
     , MonadFork        m
     , MonadMask        m
     , MonadTimer       m
     , MonadThrow  (STM m)
     , Exception failure
     , ShowProxy ps
     , forall (st' :: ps) sing. sing ~ StateToken st' => Show sing
     )
  => m (Channel m bytes, Channel m bytes)
  -> Tracer m (Role, TraceSendRecv ps)
  -> Codec ps failure m bytes
  -> ProtocolSizeLimits ps bytes
  -> ProtocolTimeLimits ps
  -> Peer ps             pr  NonPipelined st m a
  -> Peer ps (FlipAgency pr) NonPipelined st m b
  -> m (a, b)
runConnectedPeersWithLimits createChannels tracer codec slimits tlimits client server =
    createChannels >>= \(clientChannel, serverChannel) ->

    (do labelThisThread "client"
        fst <$> runPeerWithLimits
                        tracerClient codec slimits tlimits
                                     clientChannel client)
      `concurrently`
    (do labelThisThread "server"
        fst <$> runPeer tracerServer codec serverChannel server)
  where
    tracerClient = contramap ((,) Client) tracer
    tracerServer = contramap ((,) Server) tracer


runConnectedPeersWithLimitsRnd
  :: forall ps pr st failure bytes m a b.
     ( MonadAsync       m
     , MonadFork        m
     , MonadMask        m
     , MonadTimer       m
     , MonadThrow  (STM m)
     , Exception failure
     , ShowProxy ps
     , forall (st' :: ps) sing. sing ~ StateToken st' => Show sing
     )
  => m (Channel m bytes, Channel m bytes)
  -> Tracer m (Role, TraceSendRecv ps)
  -> StdGen
  -> Codec ps failure m bytes
  -> ProtocolSizeLimits ps bytes
  -> ProtocolTimeLimitsWithRnd ps
  -> Peer ps             pr  NonPipelined st m a
  -> Peer ps (FlipAgency pr) NonPipelined st m b
  -> m (a, b)
runConnectedPeersWithLimitsRnd createChannels tracer rnd codec slimits tlimits client server =
    createChannels >>= \(clientChannel, serverChannel) ->

    (do labelThisThread "client"
        fst <$> runPeerWithLimitsRnd
                        tracerClient rnd codec slimits tlimits
                                     clientChannel client)
      `concurrently`
    (do labelThisThread "server"
        fst <$> runPeer tracerServer codec serverChannel server)
  where
    tracerClient = contramap ((,) Client) tracer
    tracerServer = contramap ((,) Server) tracer


-- | Run two 'Peer's via a pair of connected 'Channel's and a common 'Codec'.
-- The client side is using 'driverWithLimits'.
--
-- This is useful for tests and quick experiments.
--
-- The first argument is expected to create two channels that are connected,
-- for example 'createConnectedChannels'.
--
runConnectedPipelinedPeersWithLimits
  :: forall ps pr st failure bytes m a b.
     ( MonadAsync      m
     , MonadFork       m
     , MonadMask       m
     , MonadTimer      m
     , MonadThrow (STM m)
     , Exception failure
     , ShowProxy ps
     , forall (st' :: ps) sing. sing ~ StateToken st' => Show sing
     )
  => m (Channel m bytes, Channel m bytes)
  -> Tracer m (Role, TraceSendRecv ps)
  -> Codec ps failure m bytes
  -> ProtocolSizeLimits ps bytes
  -> ProtocolTimeLimits ps
  -> PeerPipelined ps    pr               st m a
  -> Peer ps (FlipAgency pr) NonPipelined st m b
  -> m (a, b)
runConnectedPipelinedPeersWithLimits createChannels tracer codec slimits tlimits client server =
    createChannels >>= \(clientChannel, serverChannel) ->

    (fst <$> runPipelinedPeerWithLimits
                     tracerClient codec slimits tlimits
                                        clientChannel client)
      `concurrently`
    (fst <$> runPeer tracerServer codec serverChannel server)
  where
    tracerClient = contramap ((,) Client) tracer
    tracerServer = contramap ((,) Server) tracer


runConnectedPipelinedPeersWithLimitsRnd
  :: forall ps pr st failure bytes m a b.
     ( MonadAsync      m
     , MonadFork       m
     , MonadMask       m
     , MonadTimer      m
     , MonadThrow (STM m)
     , Exception failure
     , ShowProxy ps
     , forall (st' :: ps) sing. sing ~ StateToken st' => Show sing
     )
  => m (Channel m bytes, Channel m bytes)
  -> Tracer m (Role, TraceSendRecv ps)
  -> StdGen
  -> Codec ps failure m bytes
  -> ProtocolSizeLimits ps bytes
  -> ProtocolTimeLimitsWithRnd ps
  -> PeerPipelined ps    pr               st m a
  -> Peer ps (FlipAgency pr) NonPipelined st m b
  -> m (a, b)
runConnectedPipelinedPeersWithLimitsRnd createChannels tracer rnd codec slimits tlimits client server =
    createChannels >>= \(clientChannel, serverChannel) ->

    (fst <$> runPipelinedPeerWithLimitsRnd
                     tracerClient rnd codec slimits tlimits
                                        clientChannel client)
      `concurrently`
    (fst <$> runPeer tracerServer codec serverChannel server)
  where
    tracerClient = contramap ((,) Client) tracer
    tracerServer = contramap ((,) Server) tracer
