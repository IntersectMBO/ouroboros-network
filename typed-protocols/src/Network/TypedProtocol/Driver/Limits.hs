{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE BangPatterns #-}

-- | Drivers for running 'Peer's.
--
module Network.TypedProtocol.Driver.Limits (

  -- * Limits
  ProtocolLimits(..),
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
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime
import Control.Monad.Class.MonadTimer
import Control.Tracer (Tracer (..), traceWith)

import Network.TypedProtocol.Driver.General
import Network.TypedProtocol.Driver.Simple (TraceSendRecv(..))

import Network.TypedProtocol.Core
import Network.TypedProtocol.Pipelined
import Network.TypedProtocol.Channel
import Network.TypedProtocol.Codec


data ProtocolLimits ps bytes = ProtocolLimits {
       timeLimitForState :: forall (pr :: PeerRole) (st :: ps).
                            PeerHasAgency pr st -> Maybe DiffTime,

       sizeLimitForState :: forall (pr :: PeerRole) (st :: ps).
                            PeerHasAgency pr st -> Word,

       dataSize          :: bytes -> Word
     }

data ProtocolLimitFailure = ExceededSizeLimit
                          | ExceededTimeLimit
  deriving Show

instance Exception ProtocolLimitFailure


driverWithLimits :: forall ps peerid failure bytes m.
                    (MonadThrow m, MonadTimer m, Exception failure)
                 => Tracer m (TraceSendRecv ps peerid failure)
                 -> peerid
                 -> Codec ps failure m bytes
                 -> ProtocolLimits ps bytes
                 -> Channel m bytes
                 -> Driver ps (Maybe bytes) m
driverWithLimits tr peerid Codec{encode, decode}
                 ProtocolLimits{timeLimitForState, sizeLimitForState, dataSize}
                 channel@Channel{send} =
    Driver { sendMessage, recvMessage, startDState = Nothing }
  where
    sendMessage :: forall (pr :: PeerRole) (st :: ps) (st' :: ps).
                   PeerHasAgency pr st
                -> Message ps st st'
                -> m ()
    sendMessage stok msg = do
      send (encode stok msg)
      traceWith tr (TraceSendMsg peerid (AnyMessage msg))

    recvMessage :: forall (pr :: PeerRole) (st :: ps).
                   PeerHasAgency pr st
                -> Maybe bytes
                -> m (SomeMessage st, Maybe bytes)
    recvMessage stok trailing = do
      decoder <- decode stok
      let sizeLimit = sizeLimitForState stok
          timeLimit = fromMaybe (-1) (timeLimitForState stok)
      result  <- timeout timeLimit $
                   runDecoderWithLimit sizeLimit dataSize
                                       channel trailing decoder
      case result of
        Just (Right x@(SomeMessage msg, _trailing')) -> do
          traceWith tr (TraceRecvMsg peerid (AnyMessage msg))
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


runPeerWithLimits
  :: forall ps (st :: ps) pr peerid failure bytes m a .
     (MonadTimer m, MonadThrow m, Exception failure)
  => Tracer m (TraceSendRecv ps peerid failure)
  -> peerid
  -> Codec ps failure m bytes
  -> ProtocolLimits ps bytes
  -> Channel m bytes
  -> Peer ps pr st m a
  -> m a
runPeerWithLimits tr peerid codec limits channel peer =
    fst <$> runPeerWithDriver driver peer (startDState driver)
  where
    driver = driverWithLimits tr peerid codec limits channel


-- | Run a pipelined peer with the given channel via the given codec.
--
-- This runs the peer to completion (if the protocol allows for termination).
--
-- Unlike normal peers, running pipelined peers rely on concurrency, hence the
-- 'MonadSTM' constraint.
--
runPipelinedPeerWithLimits
  :: forall ps (st :: ps) pr peerid failure bytes m a.
     (MonadAsync m, MonadTimer m, MonadThrow m, Exception failure)
  => Tracer m (TraceSendRecv ps peerid failure)
  -> peerid
  -> Codec ps failure m bytes
  -> ProtocolLimits ps bytes
  -> Channel m bytes
  -> PeerPipelined ps pr st m a
  -> m a
runPipelinedPeerWithLimits tr peerid codec limits channel peer =
    fst <$> runPipelinedPeerWithDriver driver peer (startDState driver)
  where
    driver = driverWithLimits tr peerid codec limits channel

