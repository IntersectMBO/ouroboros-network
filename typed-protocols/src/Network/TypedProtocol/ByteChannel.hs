{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.TypedProtocol.ByteChannel where

import           Control.Exception

import           Control.Monad.Class.MonadThrow (MonadThrow (..))
import           Control.Tracer (Tracer, traceWith)


import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.Driver (TraceSendRecv (..))


-- |
-- @ByteChannel@ which allows to read a number of bytes.  @recvL@ must return @len@ bytes which is less than the given number of bytes.
--
data ByteChannel m bytes = ByteChannel {
    sendL      :: bytes -> m (),
    recvL      :: Int   -> m (Maybe bytes),
    byteLength :: bytes -> Int
  }


data DecoderFailureOrTooMuchInput failure
  = DecoderFailure !failure
  | TooMuchInput
  deriving (Show)

instance Exception failure => Exception (DecoderFailureOrTooMuchInput failure)

runDecoderWithByteChannel
    :: forall m bytes failure a. Monad m
    => Int
    -- ^ size limit for message
    -> ByteChannel m bytes
    -> Maybe bytes
    -> DecodeStep bytes failure m a
    -> m (Either (DecoderFailureOrTooMuchInput failure) (a, Maybe bytes))
runDecoderWithByteChannel limit ByteChannel{recvL, byteLength} mbytes = go (limit - maybe 0 byteLength mbytes) mbytes
    where
      go :: Int
         -> Maybe bytes
         -> DecodeStep bytes failure m a
         -> m (Either (DecoderFailureOrTooMuchInput failure) (a, Maybe bytes))
      -- we run over the limit, return an error
      go !l _  _                         | l < 0 = return (Left TooMuchInput)
      go !_ _  (DecodeDone x trailing)   = return (Right (x, trailing))
      go !_ _  (DecodeFail failure)      = return (Left (DecoderFailure failure))

      go !l Nothing         (DecodePartial k) =
        -- we can only read more @l@ bytes from the channel for this message
        recvL l >>= (\mbs -> k mbs >>= go (l - maybe 0 byteLength mbs) Nothing)

      go !l (Just trailing) (DecodePartial k) =
        k (Just trailing) >>= go l Nothing


-- |
-- Run @Peer@ over a @ByteChannel@.  Each message is required to be less than
-- @limit@ bytes, if limit is exceeded throw @'TooMuchInput'@ exception.
--
runPeerL
  :: forall ps (st :: ps) pr bytes failure m a .
     (MonadThrow m, Exception failure)
  => Int
  -- ^ limit of bytes per message
  -> Tracer m (TraceSendRecv ps)
  -> Codec ps failure m bytes
  -> ByteChannel m bytes
  -> Peer ps pr st m a
  -> m a

runPeerL limit tr Codec{encode, decode} channel@ByteChannel{sendL} =
    go Nothing
  where
    go :: forall st'.
          Maybe bytes
       -> Peer ps pr st' m a
       -> m a
    go trailing (Effect k) = k >>= go trailing
    go _        (Done _ x) = return x

    go trailing (Yield stok msg k) = do
      traceWith tr (TraceSendMsg (AnyMessage msg))
      sendL (encode stok msg)
      go trailing k

    go trailing (Await stok k) = do
      decoder <- decode stok
      res <- runDecoderWithByteChannel limit channel trailing decoder
      case res of
        Right (SomeMessage msg, trailing') -> do
          traceWith tr (TraceRecvMsg (AnyMessage msg))
          go trailing' (k msg)
        Left failure ->
          throwM failure

