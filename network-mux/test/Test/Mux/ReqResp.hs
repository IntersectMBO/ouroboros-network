{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A simple ReqResp protocol, togetether with an incremental decoder.  Mux is
-- chopping messages into 'MuxSDU's of a fixed size, for that reasone the
-- peer awaiting for messages needs an incremental decoder to assemple messages
-- back.
--
module Test.Mux.ReqResp where

import           Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as CBOR hiding (Done, Fail)
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import           Codec.Serialise (Serialise (..), serialise)
import           Control.Monad.ST
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import           Control.Monad.Class.MonadST
import           Control.Tracer (Tracer, traceWith)

import           Network.Mux.Channel

-- | Protocol messages.
--
data MsgReqResp req resp =

   -- | Client request.
   --
     MsgReq req

   -- | Server response.
   --
   | MsgResp resp

   -- | Client finishes the protocol exchange.
   --
   | MsgDone
   deriving (Show, Eq, Ord)


instance (Serialise req, Serialise resp) => Serialise (MsgReqResp req resp) where
    encode (MsgReq req)   = CBOR.encodeListLen 2 <> CBOR.encodeWord 0 <> encode req
    encode (MsgResp resp) = CBOR.encodeListLen 2 <> CBOR.encodeWord 1 <> encode resp
    encode MsgDone        = CBOR.encodeListLen 1 <> CBOR.encodeWord 2

    decode = do
      len <- CBOR.decodeListLen
      tag <- CBOR.decodeWord
      case (len, tag) of
        (2, 0) -> MsgReq  <$> decode
        (2, 1) -> MsgResp <$> decode
        (1, 2) -> pure MsgDone
        _      -> fail $ "decode MsgReqResp: unknown tag " ++ show tag


-- | A Client which requests 'req' data and receives 'resp'.
--
data ReqRespClient req resp m a where
  SendMsgReq  :: req
              -> (resp -> m (ReqRespClient req resp m a))
              -> ReqRespClient req resp m a

  SendMsgDone :: m a -> ReqRespClient req resp m a

  EarlyExit   ::   a -> ReqRespClient req resp m a


data TraceSendRecv msg
  = TraceSend msg
  | TraceRecv msg
  | TraceEarlyExit
  | TraceFailure CBOR.DeserialiseFailure
  deriving Show


runDecoderWithChannel :: forall s m a.
                         MonadST m
                      => (forall b. ST s b -> m b)
                      -> Channel m
                      -> Maybe LBS.ByteString
                      -> Decoder s a
                      -> m (Either CBOR.DeserialiseFailure (a, Maybe LBS.ByteString))

runDecoderWithChannel liftST Channel{recv} trailing decoder =
    liftST (CBOR.deserialiseIncremental decoder) >>= go (LBS.toStrict <$> trailing)
  where

    go :: Maybe BS.ByteString
       -> CBOR.IDecode s a
       -> m (Either CBOR.DeserialiseFailure (a, Maybe LBS.ByteString))

    go Nothing (CBOR.Partial k) =
      recv >>= liftST . k . fmap LBS.toStrict >>= go Nothing
    go (Just bs) (CBOR.Partial k)  =
      liftST (k (Just bs)) >>= go Nothing
    go _ (CBOR.Done trailing' _ a) | BS.null trailing'
                                   = return (Right (a, Nothing))
                                   | otherwise
                                   = return (Right (a, Just $ LBS.fromStrict trailing'))
    go _ (CBOR.Fail _ _ failure)   = return $ Left failure



-- | Run a client using a byte 'Channel'.
--
runClient :: forall req resp m a.
             ( MonadST m
             , Serialise req
             , Serialise resp
             , Show req
             , Show resp
             )
          => Tracer m (TraceSendRecv (MsgReqResp req resp))
          -> Channel m
          -> ReqRespClient req resp m a
          -> m (a, Maybe LBS.ByteString)

runClient tracer channel@Channel {send} =
    go Nothing
  where
    go :: Maybe LBS.ByteString
       -> ReqRespClient req resp m a
       -> m (a, Maybe LBS.ByteString)
    go trailing (SendMsgReq req mnext) = do
      let msg :: MsgReqResp req resp
          msg = MsgReq req
      traceWith tracer (TraceSend msg)
      send $ serialise msg

      res <- withLiftST $ \liftST -> runDecoderWithChannel
                                        liftST channel trailing decode

      case res of
        Left err -> do
          traceWith tracer (TraceFailure err)
          error $ "runClient: deserialise error: " ++ show err

        Right (msg'@(MsgResp resp), trailing') -> do
          traceWith tracer (TraceRecv msg')
          mnext resp >>= go trailing'

        Right (msg', _) -> error $ "runClient: wrong message " ++ show msg'

    go trailing (SendMsgDone ma) = do
        let msg :: MsgReqResp req resp
            msg = MsgDone
        traceWith tracer (TraceSend msg)
        send (serialise msg)
        a <- ma
        return (a, trailing)

    go trailing (EarlyExit a) = do
      traceWith tracer TraceEarlyExit
      return (a, trailing)


-- | Server which receives 'req' and responds with 'resp'.
--
data ReqRespServer req resp m a = ReqRespServer {
    -- | The client sent us a ping message. We have no choices here, and
    -- the response is nullary, all we have are local effects.
    recvMsgReq  :: req -> m (resp, ReqRespServer req resp m a)

    -- | The client terminated. Here we have a pure return value, but we
    -- could have done another action in 'm' if we wanted to.
  , recvMsgDone :: m a
  }


runServer :: forall req resp m a.
             ( MonadST m
             , Serialise req
             , Serialise resp
             , Show req
             , Show resp
             )
          => Tracer m (TraceSendRecv (MsgReqResp req resp))
          -> Channel m
          -> ReqRespServer req resp m a
          -> m (a, Maybe LBS.ByteString)

runServer tracer channel@Channel {send} =
    go Nothing
  where
    go :: Maybe LBS.ByteString
       -> ReqRespServer req resp m a
       -> m (a, Maybe LBS.ByteString)
    go trailing ReqRespServer {recvMsgReq, recvMsgDone} = do
      res <- withLiftST $ \liftST -> runDecoderWithChannel
                                        liftST channel trailing decode
      case res of
        Left err -> do
          traceWith tracer (TraceFailure err)
          error $ "runServer: deserialise error " ++ show err

        Right (msg@(MsgReq req), trailing') -> do
          traceWith tracer (TraceRecv msg)
          (resp, server) <- recvMsgReq req
          let msg' :: MsgReqResp req resp
              msg' = MsgResp resp
          traceWith tracer (TraceSend msg')
          send $ serialise msg'
          go trailing' server

        Right (msg@MsgDone, trailing') -> do
          traceWith tracer (TraceRecv msg)
          x <- recvMsgDone
          return (x, trailing')

        Right (msg, _) -> do
          traceWith tracer (TraceRecv msg)
          error $ "runServer: unexpected message " ++ show msg
