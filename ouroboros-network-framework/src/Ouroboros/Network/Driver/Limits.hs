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
{-# LANGUAGE UndecidableInstances  #-}

-- | Drivers for running 'Peer's.
--
module Ouroboros.Network.Driver.Limits
  ( -- * Limits
    ProtocolSizeLimits (..)
  , ProtocolTimeLimits (..)
  , ProtocolLimitFailure (..)
    -- * Normal peers
  , runPeerWithLimits
  , runAnnotatedPeerWithLimits
  , TraceSendRecv (..)
    -- * Pipelined peers
  , runPipelinedPeerWithLimits
  , runPipelinedAnnotatedPeerWithLimits
    -- * Driver utilities
  , driverWithLimits
  ) where

import Data.Maybe (fromMaybe)

import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer (Tracer (..), traceWith)

import Network.Mux.Timeout
import Network.TypedProtocol.Codec
import Network.TypedProtocol.Core
import Network.TypedProtocol.Driver
import Network.TypedProtocol.Pipelined

import Ouroboros.Network.Channel
import Ouroboros.Network.Driver.Simple (DecoderFailure (..), TraceSendRecv (..))
import Ouroboros.Network.Protocol.Limits
import Ouroboros.Network.Util.ShowProxy


mkDriverWithLimits :: forall ps failure bytes m f annotator.
                    ( MonadThrow m
                    , Show failure
                    , ShowProxy ps
                    , forall (st' :: ps). Show (ClientHasAgency st')
                    , forall (st' :: ps). Show (ServerHasAgency st')
                    )
                   => (forall message.
                           Word
                        -- ^ message size limit
                        -> (bytes -> Word)
                        -- ^ byte size
                        -> Channel m bytes
                        -> Maybe bytes
                        -> DecodeStep bytes failure m (f message)
                        -> m (Either (Maybe failure) (message, Maybe bytes))
                      )
                 -- ^ run incremental decoder against a channel

                 -> (forall st. annotator st -> f (SomeMessage st))
                 -- ^ transform annotator to a container holding the decoded
                 -- message

                 -> Tracer m (TraceSendRecv ps)
                 -> TimeoutFn m
                 -> CodecF ps failure m annotator bytes
                 -> ProtocolSizeLimits ps bytes
                 -> ProtocolTimeLimits ps
                 -> Channel m bytes
                 -> Driver ps (Maybe bytes) m
mkDriverWithLimits runDecodeSteps nat tracer timeoutFn
                   Codec{encode, decode}
                   ProtocolSizeLimits{sizeLimitForState, dataSize}
                   ProtocolTimeLimits{timeLimitForState}
                   channel@Channel{send} =
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
      let sizeLimit = sizeLimitForState stok
          timeLimit = fromMaybe (-1) (timeLimitForState stok)
      result  <- timeoutFn timeLimit $
                   runDecodeSteps sizeLimit dataSize
                                  channel trailing (nat <$> decoder)
      case result of
        Just (Right x@(SomeMessage msg, _trailing')) -> do
          traceWith tracer (TraceRecvMsg (AnyMessageAndAgency stok msg))
          return x
        Just (Left (Just failure)) -> throwIO (DecoderFailure stok failure)
        Just (Left Nothing)        -> throwIO (ExceededSizeLimit stok)
        Nothing                    -> throwIO (ExceededTimeLimit stok)

driverWithLimits :: forall ps failure bytes m.
                    ( MonadThrow m
                    , Show failure
                    , forall (st :: ps). Show (ClientHasAgency st)
                    , forall (st :: ps). Show (ServerHasAgency st)
                    , ShowProxy ps
                    , Monoid bytes
                    )
                 => Tracer m (TraceSendRecv ps)
                 -> TimeoutFn m
                 -> Codec ps failure m bytes
                 -> ProtocolSizeLimits ps bytes
                 -> ProtocolTimeLimits ps
                 -> Channel m bytes
                 -> Driver ps (Maybe bytes) m
driverWithLimits = mkDriverWithLimits (runDecoderWithLimit glue) const
  where
    glue _consumed _new = mempty

annotatedDriverWithLimits :: forall ps failure bytes m.
                             ( MonadThrow m
                             , Show failure
                             , forall (st :: ps). Show (ClientHasAgency st)
                             , forall (st :: ps). Show (ServerHasAgency st)
                             , ShowProxy ps
                             , Monoid bytes
                             )
                          => Tracer m (TraceSendRecv ps)
                          -> TimeoutFn m
                          -> AnnotatedCodec ps failure m bytes
                          -> ProtocolSizeLimits ps bytes
                          -> ProtocolTimeLimits ps
                          -> Channel m bytes
                          -> Driver ps (Maybe bytes) m
annotatedDriverWithLimits = mkDriverWithLimits (runDecoderWithLimit (<>)) runAnnotator

runDecoderWithLimit
    :: forall m bytes failure a. (Monad m, Monoid bytes)
    => (bytes -> bytes -> bytes)
    -> Word
    -- ^ message size limit
    -> (bytes -> Word)
    -- ^ byte size
    -> Channel m bytes
    -> Maybe bytes
    -> DecodeStep bytes failure m (bytes -> a)
    -> m (Either (Maybe failure) (a, Maybe bytes))
runDecoderWithLimit glue limit size Channel{recv} =
    go 0 <*> fromMaybe mempty
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
    go !sz _ consumed (DecodeDone toMessage trailing)
      | let sz' = sz - maybe 0 size trailing
      , sz' > limit = return $ Left Nothing
      | otherwise   = return $ Right (toMessage consumed, trailing)

    go !_ _ _ (DecodeFail failure) = return $ Left (Just failure)

    go !sz queued consumed (DecodePartial k)
      | sz > limit = return $ Left Nothing
      | otherwise  = case queued of
                       Nothing -> do mbs <- recv
                                     let !sz' = sz + maybe 0 size mbs
                                     go sz' Nothing (consumed `glue` fromMaybe mempty mbs) =<< k mbs
                       Just queued' -> do let sz' = sz + size queued'
                                          go sz' Nothing (consumed `glue` queued') =<< k (Just queued')


-- | Run a peer with the given channel via the given codec.
--
-- This runs the peer to completion (if the protocol allows for termination).
--
runPeerWithLimits
  :: forall ps (st :: ps) pr failure bytes m a .
     ( MonadAsync m
     , MonadFork m
     , MonadMask m
     , MonadThrow (STM m)
     , MonadTimer m
     , forall (st' :: ps). Show (ClientHasAgency st')
     , forall (st' :: ps). Show (ServerHasAgency st')
     , ShowProxy ps
     , Show failure
     , Monoid bytes
     )
  => Tracer m (TraceSendRecv ps)
  -> Codec ps failure m bytes
  -> ProtocolSizeLimits ps bytes
  -> ProtocolTimeLimits ps
  -> Channel m bytes
  -> Peer ps pr st m a
  -> m (a, Maybe bytes)
runPeerWithLimits tracer codec slimits tlimits channel peer =
    withTimeoutSerial $ \timeoutFn ->
      let driver = driverWithLimits tracer timeoutFn codec slimits tlimits channel
      in runPeerWithDriver driver peer (startDState driver)

-- | Run a peer with the given channel via the given annotated codec.
--
-- This runs the peer to completion (if the protocol allows for termination).
--
runAnnotatedPeerWithLimits
  :: forall ps (st :: ps) pr failure bytes m a .
     ( MonadAsync m
     , MonadFork m
     , MonadMask m
     , MonadThrow (STM m)
     , MonadTimer m
     , forall (st' :: ps). Show (ClientHasAgency st')
     , forall (st' :: ps). Show (ServerHasAgency st')
     , ShowProxy ps
     , Show failure
     , Monoid bytes
     )
  => Tracer m (TraceSendRecv ps)
  -> AnnotatedCodec ps failure m bytes
  -> ProtocolSizeLimits ps bytes
  -> ProtocolTimeLimits ps
  -> Channel m bytes
  -> Peer ps pr st m a
  -> m (a, Maybe bytes)
runAnnotatedPeerWithLimits tracer codec slimits tlimits channel peer =
    withTimeoutSerial $ \timeoutFn ->
      let driver = annotatedDriverWithLimits tracer timeoutFn codec slimits tlimits channel
      in runPeerWithDriver driver peer (startDState driver)

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
     , MonadThrow (STM m)
     , MonadTimer m
     , forall (st' :: ps). Show (ClientHasAgency st')
     , forall (st' :: ps). Show (ServerHasAgency st')
     , ShowProxy ps
     , Show failure
     , Monoid bytes
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
      in runPipelinedPeerWithDriver driver peer (startDState driver)

-- | Run a pipelined peer with the given channel via the given annotated codec.
--
-- This runs the peer to completion (if the protocol allows for termination).
--
-- Unlike normal peers, running pipelined peers rely on concurrency, hence the
-- 'MonadAsync' constraint.
--
runPipelinedAnnotatedPeerWithLimits
  :: forall ps (st :: ps) pr failure bytes m a.
     ( MonadAsync m
     , MonadFork m
     , MonadMask m
     , MonadThrow (STM m)
     , MonadTimer m
     , forall (st' :: ps). Show (ClientHasAgency st')
     , forall (st' :: ps). Show (ServerHasAgency st')
     , ShowProxy ps
     , Show failure
     , Monoid bytes
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
      in runPipelinedPeerWithDriver driver peer (startDState driver)
