{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A simple ReqResp protocol, togetether with an incremental decoder.  Mux is
-- chopping messages into 'MuxSDU's of a fixed size, for that reasone the
-- peer awaiting for messages needs an incremental decoder to assemple messages
-- back.
--
module Test.Mux.ReqResp
  ( MsgReqResp (..)
  , TraceSendRecv (..)

  -- * Normal Request-Response Client & Server

  -- ** Client
  , ReqRespClient (..)
  , runClientCBOR
  , runClientBin
  -- ** Server
  , ReqRespServer (..)
  , runServerCBOR
  , runServerBin

  -- * Passive Client & Active Server

  -- ** Passive Client
  , ReqRespClientBurst (..)
  , ReqRespClientLoop (..)
  , runClientBurstCBOR
  , runClientBurstBin

  -- ** Active Server
  , ReqRespServerBurst (..)
  , ReqRespServerLoop (..)
  , runServerBurstCBOR
  , runServerBurstBin
  )
  where

import Codec.CBOR.Decoding qualified as CBOR hiding (Done, Fail)
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Codec.Serialise (Serialise (..), serialise)

import Data.Binary.Put qualified as Bin
import Data.Binary.Get qualified as Bin

import Control.Monad.Primitive
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS

import Control.Monad.Class.MonadST
import Control.Tracer (Tracer, traceWith)

import Network.Mux.Channel

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
  | TraceFailure String
  deriving Show


runCBORDecoderWithChannel
  :: forall m a.
     MonadST m
  => ByteChannel m
  -> Maybe LBS.ByteString
  -> CBOR.Decoder (PrimState m) a
  -> m (Either CBOR.DeserialiseFailure (a, Maybe LBS.ByteString))

runCBORDecoderWithChannel Channel{recv} trailing decoder =
    stToIO (CBOR.deserialiseIncremental decoder) >>= go (LBS.toStrict <$> trailing)
  where

    go :: Maybe BS.ByteString
       -> CBOR.IDecode (PrimState m) a
       -> m (Either CBOR.DeserialiseFailure (a, Maybe LBS.ByteString))

    go Nothing (CBOR.Partial k) =
      recv >>= stToIO . k . fmap LBS.toStrict >>= go Nothing
    go (Just bs) (CBOR.Partial k)  =
      stToIO (k (Just bs)) >>= go Nothing
    go _ (CBOR.Done trailing' _ a) | BS.null trailing'
                                   = return (Right (a, Nothing))
                                   | otherwise
                                   = return (Right (a, Just $ LBS.fromStrict trailing'))
    go _ (CBOR.Fail _ _ failure)   = return $ Left failure



encodeBin :: MsgReqResp BS.ByteString BS.ByteString -> Bin.Put
encodeBin (MsgReq req)   = Bin.putWord8 0
                        >> Bin.putWord32be (fromIntegral $ BS.length req)
                        >> Bin.putByteString req
encodeBin (MsgResp resp) = Bin.putWord8 1
                        >> Bin.putWord32be (fromIntegral $ BS.length resp)
                        >> Bin.putByteString resp
encodeBin MsgDone        = Bin.putWord8 2


decodeBin :: Bin.Get (MsgReqResp BS.ByteString BS.ByteString)
decodeBin = do
  tag <- Bin.getWord8
  case tag of
    0 -> do
      len <- Bin.getWord32be
      MsgReq <$> Bin.getByteString (fromIntegral len)
    1 -> do
      len <- Bin.getWord32be
      MsgResp <$> Bin.getByteString (fromIntegral len)
    2 -> return MsgDone
    _ -> fail $ "unexpected tag: " ++ show tag



runBinDecoderWithChannel
  :: forall m a.
     Monad m
  => ByteChannel m
  -> Maybe LBS.ByteString
  -> Bin.Get a
  -> m (Either String (a, Maybe LBS.ByteString))
runBinDecoderWithChannel Channel{recv} trailing decoder =
    go (LBS.toStrict <$> trailing) (Bin.runGetIncremental decoder)
  where
    go :: Maybe BS.ByteString
       -> Bin.Decoder a
       -> m (Either String (a, Maybe LBS.ByteString))
    go Nothing (Bin.Partial k) =
      k . fmap LBS.toStrict <$> recv >>= go Nothing
    go (Just bs) (Bin.Partial k) =
      go Nothing (k (Just bs))
    go _ (Bin.Done trailing' _ a) | BS.null trailing'
                                  = return (Right (a, Nothing))
                                  | otherwise
                                  = return (Right (a, Just $ LBS.fromStrict trailing'))
    go _ (Bin.Fail _ _ failure)   = return $ Left failure



-- | Run a client using a byte 'ByteChannel'.
--
runClient
  :: forall req resp failure m a.
     ( Monad m
     , Show req
     , Show resp
     , Show failure
     )
  => (MsgReqResp req resp -> LBS.ByteString)
  -> (    ByteChannel m
       -> Maybe LBS.ByteString
       -> m (Either failure (MsgReqResp req resp, Maybe LBS.ByteString))
     )
  -> Tracer m (TraceSendRecv (MsgReqResp req resp))
  -> ByteChannel m
  -> ReqRespClient req resp m a
  -> m (a, Maybe LBS.ByteString)

runClient runEncoder runDecoder tracer channel@Channel {send} =
    go Nothing
  where
    go :: Maybe LBS.ByteString
       -> ReqRespClient req resp m a
       -> m (a, Maybe LBS.ByteString)
    go trailing (SendMsgReq req mnext) = do
      let msg :: MsgReqResp req resp
          msg = MsgReq req
      traceWith tracer (TraceSend msg)
      send $ runEncoder msg

      res <- runDecoder channel trailing

      case res of
        Left err -> do
          traceWith tracer (TraceFailure $ show err)
          error $ "runClient: deserialise error: " ++ show err

        Right (msg'@(MsgResp resp), trailing') -> do
          traceWith tracer (TraceRecv msg')
          mnext resp >>= go trailing'

        Right (msg', _) -> error $ "runClient: wrong message " ++ show msg'

    go trailing (SendMsgDone ma) = do
        let msg :: MsgReqResp req resp
            msg = MsgDone
        traceWith tracer (TraceSend msg)
        send (runEncoder msg)
        a <- ma
        return (a, trailing)

    go trailing (EarlyExit a) = do
      traceWith tracer TraceEarlyExit
      return (a, trailing)


runClientCBOR
  :: forall req resp m a.
     ( MonadST m
     , Serialise req
     , Serialise resp
     , Show req
     , Show resp
     )
  => Tracer m (TraceSendRecv (MsgReqResp req resp))
  -> ByteChannel m
  -> ReqRespClient req resp m a
  -> m (a, Maybe LBS.ByteString)
runClientCBOR = runClient serialise (\chan trailing -> runCBORDecoderWithChannel chan trailing decode)


runClientBin
  :: forall m a. Monad m
  => Tracer m (TraceSendRecv (MsgReqResp BS.ByteString BS.ByteString))
  -> ByteChannel m
  -> ReqRespClient BS.ByteString BS.ByteString m a
  -> m (a, Maybe LBS.ByteString)
runClientBin = runClient (Bin.runPut . encodeBin) (\chan trailing -> runBinDecoderWithChannel chan trailing decodeBin)


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


runServer
  :: forall req resp failure m a.
     ( Monad m
     , Show req
     , Show resp
     , Show failure
     )
  => (MsgReqResp req resp -> LBS.ByteString)
  -> (    ByteChannel m
       -> Maybe LBS.ByteString
       -> m (Either failure (MsgReqResp req resp, Maybe LBS.ByteString))
     )
  -> Tracer m (TraceSendRecv (MsgReqResp req resp))
  -> ByteChannel m
  -> ReqRespServer req resp m a
  -> m (a, Maybe LBS.ByteString)

runServer runEncoder runDecoder tracer channel@Channel {send} =
    go Nothing
  where
    go :: Maybe LBS.ByteString
       -> ReqRespServer req resp m a
       -> m (a, Maybe LBS.ByteString)
    go trailing ReqRespServer {recvMsgReq, recvMsgDone} = do
      res <- runDecoder channel trailing
      case res of
        Left err -> do
          traceWith tracer (TraceFailure $ show err)
          error $ "runServer: deserialise error " ++ show err

        Right (msg@(MsgReq req), trailing') -> do
          traceWith tracer (TraceRecv msg)
          (resp, server) <- recvMsgReq req
          let msg' :: MsgReqResp req resp
              msg' = MsgResp resp
          traceWith tracer (TraceSend msg')
          send $ runEncoder msg'
          go trailing' server

        Right (msg@MsgDone, trailing') -> do
          traceWith tracer (TraceRecv msg)
          x <- recvMsgDone
          return (x, trailing')

        Right (msg, _) -> do
          traceWith tracer (TraceRecv msg)
          error $ "runServer: unexpected message " ++ show msg

runServerCBOR
  :: forall req resp m a.
     ( MonadST m
     , Serialise req
     , Serialise resp
     , Show req
     , Show resp
     )
  => Tracer m (TraceSendRecv (MsgReqResp req resp))
  -> ByteChannel m
  -> ReqRespServer req resp m a
  -> m (a, Maybe LBS.ByteString)
runServerCBOR = runServer serialise (\chann trailing -> runCBORDecoderWithChannel chann trailing decode)

runServerBin
  :: forall m a. Monad m
  => Tracer m (TraceSendRecv (MsgReqResp BS.ByteString BS.ByteString))
  -> ByteChannel m
  -> ReqRespServer BS.ByteString BS.ByteString m a
  -> m (a, Maybe LBS.ByteString)
runServerBin = runServer (Bin.runPut . encodeBin) (\chann trailing -> runBinDecoderWithChannel chann trailing decodeBin)


--
-- Passive Client / Active Server
--


-- | A bursty client, which sends one request and then awaits for responses from
-- the server.  The server terminates the protocol with `MsgDone`.
--
-- TODO: the client should ask for n messages and terminate the protocol.
--
data ReqRespClientBurst req resp m a =
  SendMsgReqBurst req (ReqRespClientLoop resp m a)

data ReqRespClientLoop resp m a =
  AwaitResp { handleMsgDone :: m a
            , handleMsgResp :: resp -> m (ReqRespClientLoop resp m a)
            }


runClientBurst
  :: forall req resp failure m a.
     ( Monad m
     , Show req
     , Show resp
     , Show failure
     )
  => (MsgReqResp req resp -> LBS.ByteString)
  -> (    ByteChannel m
       -> Maybe LBS.ByteString
       -> m (Either failure (MsgReqResp req resp, Maybe LBS.ByteString))
     )
  -> Tracer m (TraceSendRecv (MsgReqResp req resp))
  -> ByteChannel m
  -> ReqRespClientBurst req resp m a
  -> m (a, Maybe LBS.ByteString)

runClientBurst runEncoder runDecoder tracer
               channel@Channel {send}
               (SendMsgReqBurst req loop) = do
    let msg :: MsgReqResp req resp
        msg = MsgReq req
    send (runEncoder msg)
    go Nothing loop
  where
    go :: Maybe LBS.ByteString
       -> ReqRespClientLoop resp m a
       -> m (a, Maybe LBS.ByteString)
    go trailing AwaitResp { handleMsgDone, handleMsgResp } = do
      res <- runDecoder channel trailing

      case res of
        Left err -> do
          traceWith tracer (TraceFailure $ show err)
          error $ "runClient: deserialise error: " ++ show err

        Right (msg'@(MsgResp resp), trailing') -> do
          traceWith tracer (TraceRecv msg')
          handleMsgResp resp >>= go trailing'

        Right (msg'@MsgDone, trailing') -> do
          traceWith tracer (TraceRecv msg')
          a <- handleMsgDone
          return (a, trailing')

        Right (msg', _) -> error $ "runClient: wrong message " ++ show msg'


runClientBurstCBOR
  :: forall req resp m a.
     ( MonadST m
     , Serialise req
     , Serialise resp
     , Show req
     , Show resp
     )
  => Tracer m (TraceSendRecv (MsgReqResp req resp))
  -> ByteChannel m
  -> ReqRespClientBurst req resp m a
  -> m (a, Maybe LBS.ByteString)
runClientBurstCBOR = runClientBurst serialise (\chan trailing -> runCBORDecoderWithChannel chan trailing decode)


runClientBurstBin
  :: forall m a. Monad m
  => Tracer m (TraceSendRecv (MsgReqResp BS.ByteString BS.ByteString))
  -> ByteChannel m
  -> ReqRespClientBurst BS.ByteString BS.ByteString m a
  -> m (a, Maybe LBS.ByteString)
runClientBurstBin = runClientBurst (Bin.runPut . encodeBin) (\chan trailing -> runBinDecoderWithChannel chan trailing decodeBin)


-- | Server dual to `ReqRespClientBurst`.
--
newtype ReqRespServerBurst req resp m a where
  ReqRespServerBurst :: (req -> m (ReqRespServerLoop resp m a))
                     -> ReqRespServerBurst req resp m a

data ReqRespServerLoop resp m a where
  SendMsgResp :: resp
           -> m (ReqRespServerLoop resp m a)
           -> ReqRespServerLoop resp m a

  SendMsgDoneServer :: m a
                    -> ReqRespServerLoop resp m a


runServerBurst
  :: forall req resp failure m a.
     ( Monad m
     , Show req
     , Show resp
     , Show failure
     )
  => (MsgReqResp req resp -> LBS.ByteString)
  -> (    ByteChannel m
       -> Maybe LBS.ByteString
       -> m (Either failure (MsgReqResp req resp, Maybe LBS.ByteString))
     )
  -> Tracer m (TraceSendRecv (MsgReqResp req resp))
  -> ByteChannel m
  -> ReqRespServerBurst req resp m a
  -> m (a, Maybe LBS.ByteString)

runServerBurst runEncoder runDecoder
               tracer channel@Channel {send}
               (ReqRespServerBurst k) = do
    res <- runDecoder channel Nothing
    case res of
      Right (MsgReq req, trailing) -> k req >>= go trailing
      Right (msg, _) -> error $ "runClient: wrong message " ++ show msg
      Left err -> do
        traceWith tracer (TraceFailure $ show err)
        error $ "runClient: deserialise error: " ++ show err
  where
    go :: Maybe LBS.ByteString
       -> ReqRespServerLoop resp m a
       -> m (a, Maybe LBS.ByteString)
    go !trailing (SendMsgResp resp server) = do
      let msg :: MsgReqResp req resp
          msg = MsgResp resp
      traceWith tracer (TraceSend msg)
      send $ runEncoder msg
      server >>= go trailing

    go !trailing (SendMsgDoneServer ma) = do
      let msg = MsgDone
      traceWith tracer (TraceSend msg)
      send (runEncoder msg)
      a <- ma
      return (a, trailing)

runServerBurstCBOR
  :: forall req resp m a.
     ( MonadST m
     , Show req
     , Show resp
     , Serialise req
     , Serialise resp
     )
  => Tracer m (TraceSendRecv (MsgReqResp req resp))
  -> ByteChannel m
  -> ReqRespServerBurst req resp m a
  -> m (a, Maybe LBS.ByteString)
runServerBurstCBOR = runServerBurst serialise
                                    (\chan trailing -> runCBORDecoderWithChannel chan trailing decode)

runServerBurstBin
  :: forall m a.
     Monad m
  => Tracer m (TraceSendRecv (MsgReqResp BS.ByteString BS.ByteString))
  -> ByteChannel m
  -> ReqRespServerBurst BS.ByteString BS.ByteString m a
  -> m (a, Maybe LBS.ByteString)
runServerBurstBin = runServerBurst (Bin.runPut . encodeBin)
                                   (\chan trailing -> runBinDecoderWithChannel chan trailing decodeBin)
