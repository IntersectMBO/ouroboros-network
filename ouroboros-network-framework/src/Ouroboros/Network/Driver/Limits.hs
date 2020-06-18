{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Drivers for running 'Peer's.
--
module Ouroboros.Network.Driver.Limits (

  -- * Limits
  ProtocolSizeLimits(..),
  ProtocolTimeLimits(..),
  ProtocolLimitFailure(..),

  -- * Normal peers
  runPeerWithLimits,
  TraceSendRecv(..),

  -- * Pipelined peers
  runPipelinedPeerWithLimits,

  -- * Driver utilities
  driverWithLimits,
  ) where

import Data.Maybe (fromMaybe)

import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime
import Control.Monad.Class.MonadTimer
import Control.Tracer (Tracer (..), traceWith)

import Data.Foldable (traverse_)

import Network.Mux.Timeout
import Network.TypedProtocol.Core
import Network.TypedProtocol.Pipelined
import Network.TypedProtocol.Driver

import Ouroboros.Network.Codec
import Ouroboros.Network.Channel
import Ouroboros.Network.Driver.Simple (TraceSendRecv(..))

data ProtocolSizeLimits ps bytes = ProtocolSizeLimits {
       sizeLimitForState :: forall (pr :: PeerRole) (st :: ps).
                            PeerHasAgency pr st -> Word,

       dataSize          :: bytes -> Word
     }

data ProtocolTimeLimits ps = ProtocolTimeLimits {
       timeLimitForState :: forall (pr :: PeerRole) (st :: ps).
                            PeerHasAgency pr st -> Maybe DiffTime
     }

data ProtocolLimitFailure = ExceededSizeLimit
                          | ExceededTimeLimit
  deriving (Eq, Show)

instance Exception ProtocolLimitFailure


driverWithLimits :: forall ps failure bytes m.
                    (MonadThrow m, MonadTimer m, Exception failure)
                 => Tracer m (TraceSendRecv ps)
                 -> TimeoutFn m
                 -> Codec ps failure m bytes
                 -> ProtocolSizeLimits ps bytes
                 -> ProtocolTimeLimits ps
                 -> Channel m bytes
                 -> Driver ps (Maybe bytes) m
driverWithLimits tracer timeoutFn
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
      traceWith tracer (TraceSendMsg (AnyMessage msg))

    recvMessage :: forall (pr :: PeerRole) (st :: ps).
                   PeerHasAgency pr st
                -> Maybe bytes
                -> m (SomeMessage st, Maybe bytes)
    recvMessage stok trailing = do
      decoder <- decode stok
      let sizeLimit = sizeLimitForState stok
          timeLimit = fromMaybe (-1) (timeLimitForState stok)
      result  <- timeoutFn timeLimit $
                   runDecoderWithLimit sizeLimit dataSize
                                       channel trailing decoder
      case result of
        Just (Right x@(SomeMessage msg, _trailing')) -> do
          traceWith tracer (TraceRecvMsg (AnyMessage msg))
          return x
        Just (Left (Just failure)) -> throwM failure
        Just (Left Nothing)        -> throwM ExceededSizeLimit
        Nothing                    -> throwM ExceededTimeLimit

runDecoderWithLimit
    :: forall m bytes failure a. Monad m
    => Word
    -- ^ message size limit
    -> (bytes -> Word)
    -- ^ byte size
    -> Channel m bytes
    -> Maybe bytes
    -> DecodeStep bytes failure m a
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
       -> DecodeStep bytes failure m a
       -> m (Either (Maybe failure) (a, Maybe bytes))

    go !sz _ (DecodeDone x trailing)
      | let sz' = sz - maybe 0 size trailing
      , sz' > limit = return (Left Nothing)
      | otherwise   = return (Right (x, trailing))

    go !_ _  (DecodeFail failure) = return (Left (Just failure))

    go !sz trailing (DecodePartial k)
      | sz > limit = return (Left Nothing)
      | otherwise  = case trailing of
                       Nothing -> do mbs <- recv
                                     let !sz' = sz + maybe 0 size mbs
                                     go sz' Nothing =<< k mbs
                       Just bs -> do let sz' = sz + size bs
                                     go sz' Nothing =<< k (Just bs)


-- | Run a peer enforcing 'ProtocolSizeLimits` and 'ProtocolTimeLimits'.
-- Decoder's trailing data are pushed back to the channel.
--
runPeerWithLimits
  :: forall ps (st :: ps) pr failure bytes m a .
     (MonadAsync m, MonadFork m, MonadMask m, MonadThrow (STM m),
      MonadMonotonicTime m, MonadTimer m, Exception failure)
  => Tracer m (TraceSendRecv ps)
  -> Codec ps failure m bytes
  -> ProtocolSizeLimits ps bytes
  -> ProtocolTimeLimits ps
  -> Channel m bytes
  -> Peer ps pr st m a
  -> m a
runPeerWithLimits tracer codec slimits tlimits
                  channel@Channel{pushBackTrailingData} peer =
    withTimeoutSerial $ \timeoutFn -> do
      let driver = driverWithLimits tracer timeoutFn codec slimits tlimits channel
      (a, trailing) <- runPeerWithDriver driver peer (startDState driver)
      traverse_ pushBackTrailingData trailing
      pure a


-- | Run a pipelined peer with the given channel via the given codec.
--
-- This runs the peer to completion (if the protocol allows for termination).
-- Decoders' trailing data are pushed back to the channel.
--
-- Unlike normal peers, running pipelined peers rely on concurrency, hence the
-- 'MonadSTM' constraint.
--
runPipelinedPeerWithLimits
  :: forall ps (st :: ps) pr failure bytes m a.
     (MonadAsync m, MonadFork m, MonadMask m, MonadThrow (STM m),
      MonadMonotonicTime m, MonadTimer m, Exception failure)
  => Tracer m (TraceSendRecv ps)
  -> Codec ps failure m bytes
  -> ProtocolSizeLimits ps bytes
  -> ProtocolTimeLimits ps
  -> Channel m bytes
  -> PeerPipelined ps pr st m a
  -> m a
runPipelinedPeerWithLimits tracer codec slimits tlimits
                           channel@Channel{pushBackTrailingData} peer =
    withTimeoutSerial $ \timeoutFn -> do
      let driver = driverWithLimits tracer timeoutFn codec slimits tlimits channel
      (a, trailing) <- runPipelinedPeerWithDriver driver peer (startDState driver)
      traverse_ pushBackTrailingData trailing
      pure a
