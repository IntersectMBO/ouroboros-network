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
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.Maybe (fromMaybe)
import GHC.Stack
import System.Random

import Control.DeepSeq (NFData, force)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
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
     ( MonadMonotonicTime m
     , MonadThrow m
     , ShowProxy ps
     , forall (st' :: ps) tok. tok ~ StateToken st' => Show tok
     , Show failure
     )
  => (forall a.
           Word
        -> Channel m bytes
        -> Maybe (Reception bytes)
        -> DecodeStep bytes failure m (f a)
        -> m (Either (Maybe failure) (a, Maybe Time, Maybe (Reception bytes)))
     )
  -- ^ run incremental decoder against a channel; produces the arrival time of
  -- the last byte of the decoded message (when available) alongside the
  -- (possibly partial) next-message bytes with per-byte arrival times.

  -> (forall (st :: ps). annotator st -> f (SomeMessage st))
  -- ^ transform annotator to a container holding the decoded
  -- message

  -> Tracer m (TraceSendRecv ps)
  -> TimeoutFn m
  -> CodecF ps failure m annotator bytes
  -> ProtocolSizeLimits ps bytes
  -> ProtocolTimeLimits ps
  -> Channel m bytes
  -> Driver ps pr (Maybe (Reception bytes)) m
mkDriverWithLimits runDecodeStep nat tracer
                   timeoutFn
                   Codec{encode, decode}
                   ProtocolSizeLimits{sizeLimitForState}
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
      let sizeLimit = sizeLimitForState @st stateToken
          timeLimit = fromMaybe (-1) (timeLimitForState @st stateToken)
      result  <- timeoutFn timeLimit $
                   runDecodeStep sizeLimit
                                 channel trailing (nat <$> decoder)

      case result of
        Just (Right (SomeMessage msg, mbTm, trailing')) -> do
          traceWith tracer (TraceRecvMsg mbTm (AnyMessage msg))
          return (SomeMessage msg, trailing')
        Just (Left (Just failure)) -> throwIO (DecoderFailure tok failure)
        Just (Left Nothing)        -> throwIO (ExceededSizeLimit tok)
        Nothing                    -> throwIO (ExceededTimeLimit tok)


driverWithLimits
  :: forall ps (pr :: PeerRole) failure bytes m.
     ( MonadEvaluate m
     , MonadMonotonicTime m
     , MonadThrow m
     , ShowProxy ps
     , forall (st' :: ps) tok. tok ~ StateToken st' => Show tok
     , NFData failure
     , Show failure
     , BearerBytes bytes
     )
  => Tracer m (TraceSendRecv ps)
  -> TimeoutFn m
  -> Codec ps failure m bytes
  -> ProtocolSizeLimits ps bytes
  -> ProtocolTimeLimits ps
  -> Channel m bytes
  -> Driver ps pr (Maybe (Reception bytes)) m
driverWithLimits = mkDriverWithLimits runDecoderWithLimit Identity


annotatedDriverWithLimits
  :: forall ps (pr :: PeerRole) failure bytes m.
     ( MonadEvaluate m
     , MonadMonotonicTime m
     , MonadThrow m
     , Monoid bytes
     , ShowProxy ps
     , forall (st' :: ps) tok. tok ~ StateToken st' => Show tok
     , NFData failure
     , Show failure
     , BearerBytes bytes
     )
  => Tracer m (TraceSendRecv ps)
  -> TimeoutFn m
  -> AnnotatedCodec ps failure m bytes
  -> ProtocolSizeLimits ps bytes
  -> ProtocolTimeLimits ps
  -> Channel m bytes
  -> Driver ps pr (Maybe (Reception bytes)) m
annotatedDriverWithLimits = mkDriverWithLimits runAnnotatedDecoderWithLimit runAnnotator


mkDriverWithLimitsRnd
  :: forall ps (pr :: PeerRole) failure bytes m f annotator.
     ( MonadMonotonicTime m
     , MonadThrow m
     , ShowProxy ps
     , forall (st' :: ps) tok. tok ~ StateToken st' => Show tok
     , Show failure
     , HasCallStack
     )
  => (forall a.
           HasCallStack
        => Word
        -> Channel m bytes
        -> Maybe (Reception bytes)
        -> DecodeStep bytes failure m (f a)
        -> m (Either (Maybe failure) (a, Maybe Time, Maybe (Reception bytes)))
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
  -> Driver ps pr (Maybe (Reception bytes), StdGen) m
mkDriverWithLimitsRnd runDecodeStep nat tracer timeoutFn rnd0
                      Codec{encode, decode}
                      ProtocolSizeLimits{sizeLimitForState}
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
      tm <- getMonotonicTime
      send (encode msg)
      traceWith tracer (TraceSendMsg tm (AnyMessage msg))


    recvMessage :: forall (st :: ps).
                   StateTokenI st
                => ActiveState st
                => TheyHaveAgencyProof pr st
                -> (Maybe (Reception bytes), StdGen)
                -> m (SomeMessage st, (Maybe (Reception bytes), StdGen))
    recvMessage !_ (trailing, !rnd) = do
      let tok = stateToken
      decoder <- decode tok
      let sizeLimit = sizeLimitForState @st stateToken
          (timeLimit, rnd') = first (fromMaybe (-1))
                            $ timeLimitForStateWithRnd @st stateToken rnd
      result  <- timeoutFn timeLimit $
                   runDecodeStep sizeLimit
                                 channel trailing (nat <$> decoder)

      case result of
        Just (Right (SomeMessage msg, mbTm, trailing')) -> do
          traceWith tracer (TraceRecvMsg mbTm (AnyMessage msg))
          return (SomeMessage msg, (trailing', rnd'))
        Just (Left (Just failure)) -> throwIO (DecoderFailure tok failure)
        Just (Left Nothing)        -> throwIO (ExceededSizeLimit tok)
        Nothing                    -> throwIO (ExceededTimeLimit tok)


-- | Like 'driverWithLimits' but gives access to `StdGen` to generate
-- `ProtocolTimeouts` for the next state.
--
driverWithLimitsRnd :: forall ps (pr :: PeerRole) failure bytes m.
                       ( MonadEvaluate m
                       , MonadMonotonicTime m
                       , MonadThrow m
                       , ShowProxy ps
                       , forall (st' :: ps) tok. tok ~ StateToken st' => Show tok
                       , Show failure
                       , NFData failure
                       , HasCallStack
                       , BearerBytes bytes
                       )
                    => Tracer m (TraceSendRecv ps)
                    -> TimeoutFn m
                    -> StdGen
                    -> Codec ps failure m bytes
                    -> ProtocolSizeLimits ps bytes
                    -> ProtocolTimeLimitsWithRnd ps
                    -> Channel m bytes
                    -> Driver ps pr (Maybe (Reception bytes), StdGen) m
driverWithLimitsRnd = mkDriverWithLimitsRnd runDecoderWithLimit Identity


runDecoderWithLimit
    :: forall m bytes failure a.
       ( Monad m
       , MonadEvaluate m
       , NFData failure
       , BearerBytes bytes
       )
    => Word
    -- ^ message size limit
    -> Channel m bytes
    -> Maybe (Reception bytes)
    -> DecodeStep bytes failure m (Identity a)
    -> m (Either (Maybe failure) (a, Maybe Time, Maybe (Reception bytes)))
runDecoderWithLimit limit Channel{recv} =
    \trailing step -> go IntMap.empty 0 0 trailing step
  where
    size :: bytes -> Word
    size = bearerBytesSize

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
    go :: IntMap Time
       -> Word        -- ^ size of the latest 'Reception'
       -> Word        -- ^ size of consumed input so far
       -> Maybe (Reception bytes)
       -> DecodeStep bytes failure m (Identity a)
       -> m (Either (Maybe failure) (a, Maybe Time, Maybe (Reception bytes)))

    go !tms !rsz !sz !_ (DecodeDone (Identity x) trailing)
      | let sz' = sz - maybe 0 size trailing
      , sz' > limit = return (Left Nothing)
      | otherwise   =
        let (mbTm, tms') = runDecoderWithChannel_DecodeDone
                              tms
                              (fromIntegral rsz)
                              (fromIntegral $ maybe 0 size trailing)
        in
        return (Right (x, mbTm, MkReception tms' <$> trailing))

    go !_ !_ !_ !_ (DecodeFail failure) =
      Left . Just <$> evaluate (force failure)

    go !_ !_ !sz !trailing (DecodePartial k)
      | sz > limit = return (Left Nothing)
      | otherwise  = case trailing of
                       Nothing -> do mr <- recv
                                     let (!tms', !rsz', mbs) = case mr of
                                           Nothing                   -> (IntMap.empty, 0, Nothing)
                                           Just (MkReception tms bs) -> (tms, size bs, Just bs)
                                         !sz' = sz + rsz'
                                     go tms' rsz' sz' Nothing =<< k mbs
                       Just (MkReception tms' bs) -> do
                           -- INVARIANT This is only reachable on the first
                           -- invocation of @go@, so the incoming @tms@, @rsz@
                           -- and @sz@ are empty.
                           let !sz' = size bs
                           go tms' sz' sz' Nothing =<< k (Just bs)


runAnnotatedDecoderWithLimit
    :: forall m bytes failure a.
       ( Monad m
       , MonadEvaluate m
       , Monoid bytes
       , NFData failure
       , BearerBytes bytes
       )
    => Word
    -- ^ message size limit
    -> Channel m bytes
    -> Maybe (Reception bytes)
    -> DecodeStep bytes failure m (bytes -> a)
    -> m (Either (Maybe failure) (a, Maybe Time, Maybe (Reception bytes)))
runAnnotatedDecoderWithLimit limit Channel{recv} =
    \trailing step -> go mempty IntMap.empty 0 0 trailing step
  where
    size :: bytes -> Word
    size = bearerBytesSize

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
    go :: [bytes]                  -- ^ accumulation of chunks received from the network
       -> IntMap Time              -- ^ per-byte arrival times for the latest 'Reception'
       -> Word                     -- ^ size of the latest 'Reception'
       -> Word                     -- ^ size of consumed input so far
       -> Maybe (Reception bytes)  -- ^ any trailing data
       -> DecodeStep bytes failure m (bytes -> a)
       -> m (Either (Maybe failure) (a, Maybe Time, Maybe (Reception bytes)))


    go !_ !_ !_ !sz !_ DecodePartial {} | sz > limit =
      return (Left Nothing)

    go !bytes !_ !_ !sz (Just (MkReception tms' trailing)) (DecodePartial k) = do
      -- INVARIANT only reachable on the first invocation; previous @tms@, @rsz@
      -- carry no information yet.
      let sz' = sz + size trailing
      step <- k (Just trailing)
      go (trailing : bytes) tms' (size trailing) sz' Nothing step

    go !bytes _ _ !sz Nothing (DecodePartial k) = do
      mr <- recv
      let (!tms', !rsz', mbs) = case mr of
            Nothing                   -> (IntMap.empty, 0, Nothing)
            Just (MkReception tms bs) -> (tms, size bs, Just bs)
          !sz' = sz + rsz'
      step <- k mbs
      go (case mbs of
            Nothing -> bytes
            Just bs -> bs : bytes)
         tms' rsz' sz' Nothing step

    go !bytes tms rsz !sz !_ (DecodeDone f trailing)
      | let sz' = sz - maybe 0 size trailing
      , sz' > limit = return (Left Nothing)
      | otherwise   =
        let (mbTm, tms') = runDecoderWithChannel_DecodeDone
                              tms
                              (fromIntegral rsz)
                              (fromIntegral $ maybe 0 size trailing)
        in
        return (Right (f (mconcat $ reverse bytes), mbTm, MkReception tms' <$> trailing))

    go !_ !_ !_ !_ !_ (DecodeFail failure) =
      Left . Just <$> evaluate (force failure)


-- | Run a peer with limits.
--
runPeerWithLimits
  :: forall ps (st :: ps) pr failure bytes m a .
     ( MonadAsync m
     , MonadEvaluate m
     , MonadFork m
     , MonadMask m
     , MonadThrow (STM m)
     , MonadTimer m
     , ShowProxy ps
     , forall (st' :: ps) stok. stok ~ StateToken st' => Show stok
     , NFData a
     , NFData failure
     , Show failure
     , BearerBytes bytes
     )
  => Tracer m (TraceSendRecv ps)
  -> Codec ps failure m bytes
  -> ProtocolSizeLimits ps bytes
  -> ProtocolTimeLimits ps
  -> Channel m bytes
  -> Peer ps pr NonPipelined st m a
  -> m (a, Maybe (Reception bytes))
runPeerWithLimits tracer codec slimits tlimits channel peer =
    withTimeoutSerial $ \timeoutFn ->
      let driver = driverWithLimits tracer timeoutFn codec slimits tlimits channel
      in runPeerWithDriver driver peer


-- | Run an peer with annotated codec & limits.
--
runAnnotatedPeerWithLimits
  :: forall ps (st :: ps) pr failure bytes m a .
     ( MonadAsync m
     , MonadEvaluate m
     , MonadFork m
     , MonadMask m
     , MonadThrow (STM m)
     , MonadTimer m
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
  -> ProtocolSizeLimits ps bytes
  -> ProtocolTimeLimits ps
  -> Channel m bytes
  -> Peer ps pr NonPipelined st m a
  -> m (a, Maybe (Reception bytes))
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
     , MonadEvaluate m
     , MonadFork m
     , MonadMask m
     , MonadThrow (STM m)
     , MonadTimer m
     , ShowProxy ps
     , forall (st' :: ps) stok. stok ~ StateToken st' => Show stok
     , NFData a
     , NFData failure
     , Show failure
     , HasCallStack
     , BearerBytes bytes
     )
  => Tracer m (TraceSendRecv ps)
  -> StdGen
  -> Codec ps failure m bytes
  -> ProtocolSizeLimits ps bytes
  -> ProtocolTimeLimitsWithRnd ps
  -> Channel m bytes
  -> Peer ps pr NonPipelined st m a
  -> m (a, Maybe (Reception bytes))
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
     , MonadEvaluate m
     , MonadFork m
     , MonadMask m
     , MonadTimer m
     , MonadThrow (STM m)
     , ShowProxy ps
     , forall (st' :: ps) stok. stok ~ StateToken st' => Show stok
     , NFData a
     , NFData failure
     , Show failure
     , BearerBytes bytes
     )
  => Tracer m (TraceSendRecv ps)
  -> Codec ps failure m bytes
  -> ProtocolSizeLimits ps bytes
  -> ProtocolTimeLimits ps
  -> Channel m bytes
  -> PeerPipelined ps pr st m a
  -> m (a, Maybe (Reception bytes))
runPipelinedPeerWithLimits tracer codec slimits tlimits channel peer =
    withTimeoutSerial $ \timeoutFn ->
      let driver = driverWithLimits tracer timeoutFn codec slimits tlimits channel
      in runPipelinedPeerWithDriver driver peer


runPipelinedAnnotatedPeerWithLimits
  :: forall ps (st :: ps) pr failure bytes m a.
     ( MonadAsync m
     , MonadEvaluate m
     , MonadFork m
     , MonadMask m
     , MonadTimer m
     , MonadThrow (STM m)
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
  -> ProtocolSizeLimits ps bytes
  -> ProtocolTimeLimits ps
  -> Channel m bytes
  -> PeerPipelined ps pr st m a
  -> m (a, Maybe (Reception bytes))
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
     , MonadEvaluate m
     , MonadFork m
     , MonadMask m
     , MonadTimer m
     , MonadThrow (STM m)
     , ShowProxy ps
     , forall (st' :: ps) stok. stok ~ StateToken st' => Show stok
     , NFData a
     , NFData failure
     , Show failure
     , BearerBytes bytes
     )
  => Tracer m (TraceSendRecv ps)
  -> StdGen
  -> Codec ps failure m bytes
  -> ProtocolSizeLimits ps bytes
  -> ProtocolTimeLimitsWithRnd ps
  -> Channel m bytes
  -> PeerPipelined ps pr st m a
  -> m (a, Maybe (Reception bytes))
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
     , MonadEvaluate    m
     , MonadFork        m
     , MonadMask        m
     , MonadTimer       m
     , MonadThrow  (STM m)
     , NFData a
     , NFData b
     , Exception failure
     , NFData failure
     , ShowProxy ps
     , forall (st' :: ps) sing. sing ~ StateToken st' => Show sing
     , BearerBytes bytes
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
     , MonadEvaluate    m
     , MonadFork        m
     , MonadMask        m
     , MonadTimer       m
     , MonadThrow  (STM m)
     , NFData a
     , NFData b
     , Exception failure
     , NFData failure
     , ShowProxy ps
     , forall (st' :: ps) sing. sing ~ StateToken st' => Show sing
     , HasCallStack
     , BearerBytes bytes
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
     , MonadEvaluate   m
     , MonadFork       m
     , MonadMask       m
     , MonadTimer      m
     , MonadThrow (STM m)
     , Exception failure
     , NFData a
     , NFData b
     , NFData failure
     , ShowProxy ps
     , forall (st' :: ps) sing. sing ~ StateToken st' => Show sing
     , BearerBytes bytes
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
     , MonadEvaluate   m
     , MonadFork       m
     , MonadMask       m
     , MonadTimer      m
     , MonadThrow (STM m)
     , Exception failure
     , NFData a
     , NFData b
     , NFData failure
     , ShowProxy ps
     , forall (st' :: ps) sing. sing ~ StateToken st' => Show sing
     , BearerBytes bytes
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
