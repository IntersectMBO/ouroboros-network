{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Driver
  ( runPeerWithByteLimit
  , runPipelinedPeerWithByteLimit

  , DecoderFailureOrTooMuchInput (..)
  , ByteLimit (..)
  , runDecoderWithChannelByteLimits
  )where

import           Control.Exception (Exception)
import           Data.Int (Int64)

import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSTM
import           Control.Tracer (Tracer)

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Channel
import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.Pipelined
import           Network.TypedProtocol.Driver


data ByteLimit bytes = ByteLimit {
      byteLimit  :: !Int64,
      byteLength :: !(bytes -> Int64)
    }

data DecoderFailureOrTooMuchInput failure
  = DecoderFailure !failure
  | TooMuchInput
  deriving (Show)

instance Exception failure => Exception (DecoderFailureOrTooMuchInput failure)

-- | Run a codec incremental decoder 'DecodeStep' against a channel. It also
-- takes any extra input data and returns any unused trailing data.
--
runDecoderWithChannelByteLimits
    :: forall m bytes failure a. Monad m
    => ByteLimit bytes
    -> Channel m bytes
    -> Maybe bytes
    -> DecodeStep bytes failure m a
    -> m (Either (DecoderFailureOrTooMuchInput failure) (a, Maybe bytes))

runDecoderWithChannelByteLimits ByteLimit {byteLimit, byteLength} Channel{recv} mbytes = go 0 mbytes
    where
      go :: Int64
         -- ^ length of consumed input
         -> Maybe bytes
         -> DecodeStep bytes failure m a
         -> m (Either (DecoderFailureOrTooMuchInput failure) (a, Maybe bytes))
      -- we decoded the data, but we might be over the limit
      go !l _  (DecodeDone x trailing) | l - maybe 0 byteLength trailing > byteLimit
                                       = return (Left TooMuchInput)
                                       | otherwise
                                       = return (Right (x, trailing))
      -- we run over the limit, return @TooMuchInput@ error
      go !l _  _                       | l > byteLimit
                                       = return (Left TooMuchInput)
      go !_ _  (DecodeFail failure)    = return (Left (DecoderFailure failure))

      go !l Nothing         (DecodePartial k) =
        recv >>= (\mbs -> k mbs >>= go (l + maybe 0 byteLength mbs) Nothing)

      go !l (Just trailing) (DecodePartial k) =
        k (Just trailing) >>= go (l + byteLength trailing) Nothing

-- |
-- Like @'runPeer'@, but require that each inbound message is smaller than
-- @limit@ bytes; if the limit is exceeded throw @'TooMuchInput'@ exception.
--
runPeerWithByteLimit
  :: forall ps (st :: ps) pr bytes peerid failure m a .
     (MonadThrow m, Exception failure)
  => ByteLimit bytes
  -> Tracer m (TraceSendRecv ps peerid (DecoderFailureOrTooMuchInput failure))
  -> Codec ps failure m bytes
  -> peerid
  -> Channel m bytes
  -> Peer ps pr st m a
  -> m a

runPeerWithByteLimit limit tr codec peerid channel =
  runPeerWith tr codec peerid channel (\_stok -> runDecoderWithChannelByteLimits limit)


runPipelinedPeerWithByteLimit
  :: forall ps (st :: ps) pr peerid failure bytes m a.
     (MonadSTM m, MonadAsync m, MonadCatch m, Exception failure)
  => ByteLimit bytes
  -> Tracer m (TraceSendRecv ps peerid (DecoderFailureOrTooMuchInput failure))
  -> Codec ps failure m bytes
  -> peerid
  -> Channel m bytes
  -> PeerPipelined ps pr st m a
  -> m a

runPipelinedPeerWithByteLimit limit tr codec peerid channel =
  runPipelinedPeerWith tr codec peerid channel (\_stok -> runDecoderWithChannelByteLimits limit)
