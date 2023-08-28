{-# OPTIONS_GHC -Wno-deprecations #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Cardano.KESAgent.Protocols.Service.Driver
  where

import Cardano.KESAgent.KES.Crypto
import Cardano.KESAgent.KES.OCert
import Cardano.KESAgent.Util.Pretty
import Cardano.KESAgent.Protocols.Service.Protocol
import Cardano.KESAgent.Protocols.VersionedProtocol
import Cardano.KESAgent.Util.RefCounting

import Cardano.Binary
import Cardano.Crypto.DirectSerialise
import Cardano.Crypto.KES.Class
import Cardano.Crypto.Libsodium.Memory
  ( allocaBytes
  , copyMem
  , packByteStringCStringLen
  , unpackByteStringCStringLen
  )

import Ouroboros.Network.RawBearer

import Control.Monad ( void, when )
import Control.Monad.Class.MonadMVar
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow ( MonadThrow, bracket )
import Control.Tracer ( Tracer, traceWith )
import Data.Binary ( decode, encode )
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Functor.Contravariant ( (>$<) )
import Data.Proxy
import Data.Typeable
import Data.Word
import Foreign ( Ptr, castPtr, plusPtr )
import Foreign.C.Types ( CChar, CSize )
import Foreign.Marshal.Alloc ( free, mallocBytes )
import Foreign.Marshal.Utils ( copyBytes )
import Network.TypedProtocol.Core
import Network.TypedProtocol.Driver
import Text.Printf

-- | Logging messages that the Driver may send
data ServiceDriverTrace
  = ServiceDriverSendingVersionID VersionIdentifier
  | ServiceDriverReceivingVersionID
  | ServiceDriverReceivedVersionID VersionIdentifier
  | ServiceDriverSendingKey Word64
  | ServiceDriverSentKey Word64
  | ServiceDriverReceivingKey
  | ServiceDriverReceivedKey Word64
  | ServiceDriverConfirmingKey
  | ServiceDriverConfirmedKey
  | ServiceDriverDecliningKey
  | ServiceDriverDeclinedKey
  | ServiceDriverConnectionClosed
  | ServiceDriverCRefEvent CRefEvent
  | ServiceDriverMisc String
  deriving (Show)

instance Pretty ServiceDriverTrace where
  pretty (ServiceDriverMisc x) = x
  pretty x = drop (strLength "ServiceDriver") (show x)

serviceDriver :: forall c m f t p
               . Crypto c
              => Typeable c
              => VersionedProtocol (ServiceProtocol m c)
              => KESAlgorithm (KES c)
              => DirectDeserialise m (SignKeyKES (KES c))
              => DirectSerialise m (SignKeyKES (KES c))
              => MonadThrow m
              => MonadSTM m
              => MonadMVar m
              => MonadFail m
              => MonadST m
              => RawBearer m
              -> Tracer m ServiceDriverTrace
              -> Driver (ServiceProtocol m c) () m
serviceDriver s tracer = Driver
  { sendMessage = \agency msg -> case (agency, msg) of
      (ServerAgency TokInitial, VersionMessage) -> do
        let VersionIdentifier v = versionIdentifier (Proxy @(ServiceProtocol m c))
        sendVersion (Proxy @(ServiceProtocol m c)) s (ServiceDriverSendingVersionID >$< tracer)
      (ServerAgency TokIdle, KeyMessage skpRef oc) -> do
        traceWith tracer $ ServiceDriverSendingKey (ocertN oc)
        sendBundle s skpRef oc
        traceWith tracer $ ServiceDriverSentKey (ocertN oc)
      (ServerAgency TokIdle, EndMessage) -> do
        return ()
      (ClientAgency TokWaitForConfirmation, RecvResultMessage reason) -> do
        if reason == RecvOK then
          traceWith tracer ServiceDriverConfirmingKey
        else
          traceWith tracer ServiceDriverDecliningKey
        void $ sendWord32 s (encodeRecvResult reason)

  , recvMessage = \agency () -> case agency of
      (ServerAgency TokInitial) -> do
        traceWith tracer ServiceDriverReceivingVersionID
        result <- receiveVersion (Proxy @(ServiceProtocol m c)) s (ServiceDriverReceivedVersionID >$< tracer)
        case result of
          Right _ ->  
            return (SomeMessage VersionMessage, ())
          Left (expectedV, v) ->
            fail $ "DRIVER: Invalid version, expected " ++ show expectedV ++ ", but got " ++ show v
      (ServerAgency TokIdle) -> do
        traceWith tracer ServiceDriverReceivingKey
        result <- receiveBundle s (ServiceDriverMisc >$< tracer)
        case result of
          Just (skpVar, oc) -> do
            traceWith tracer $ ServiceDriverReceivedKey (ocertN oc)
            return (SomeMessage (KeyMessage skpVar oc), ())
          Nothing -> do
            traceWith tracer ServiceDriverConnectionClosed
            return (SomeMessage EndMessage, ())
      (ClientAgency TokWaitForConfirmation) -> do
        result <- maybe RecvErrorUnknown decodeRecvResult <$> receiveWord32 s
        if result == RecvOK then
          traceWith tracer ServiceDriverConfirmedKey
        else
          traceWith tracer ServiceDriverDeclinedKey
        return (SomeMessage (RecvResultMessage result), ())

  , startDState = ()
  }

sendVersion :: ( VersionedProtocol protocol
               , MonadST m
               , MonadThrow m
               )
            => Proxy protocol
            -> RawBearer m
            -> Tracer m VersionIdentifier
            -> m ()
sendVersion p s tracer = do
  traceWith tracer versionID
  void $ sendBS s (unVersionIdentifier versionID)
  where
    versionID = versionIdentifier p

receiveVersion :: forall protocol m.
                  ( VersionedProtocol protocol
                  , MonadST m
                  , MonadThrow m
                  , MonadFail m
                  )
               => Proxy protocol
               -> RawBearer m
               -> Tracer m VersionIdentifier
               -> m (Either (VersionIdentifier, VersionIdentifier) VersionIdentifier)
receiveVersion p s tracer = do
  v <- VersionIdentifier <$> receiveBS s versionIdentifierLength
  traceWith tracer v
  let expectedV = versionIdentifier p
  if v == expectedV then
    return $ Right v
  else
    return $ Left (expectedV, v)

sendBundle :: ( MonadST m
              , MonadSTM m
              , MonadThrow m
              , Crypto c
              , Typeable c
              , DirectSerialise m (SignKeyKES (KES c))
              )
           => RawBearer m
           -> CRef m (SignKeyWithPeriodKES (KES c))
           -> OCert c
           -> m Int
sendBundle s skpRef oc =
  withCRefValue skpRef $ \(SignKeyWithPeriodKES sk t) -> do
    directSerialise (\buf bufSize -> do
          n <- send s (castPtr buf) (fromIntegral bufSize)
          when (fromIntegral n /= bufSize) (error "AAAAA")
        ) sk
    let serializedOC = serialize' oc
    sendWord32 s (fromIntegral t)
    sendWord32 s (fromIntegral (BS.length serializedOC))
    sendBS s serializedOC

receiveBundle :: ( MonadST m
                 , MonadSTM m
                 , MonadMVar m
                 , MonadThrow m
                 , Crypto c
                 , Typeable c
                 , DirectDeserialise m (SignKeyKES (KES c))
                 )
              => RawBearer m
              -> Tracer m String
              -> m (Maybe (CRef m (SignKeyWithPeriodKES (KES c)), OCert c))
receiveBundle s tracer = do
  noReadVar <- newEmptyMVar
  traceWith tracer "waiting for sign key bytes..."
  sk <- directDeserialise
          (\buf bufSize -> do
              traceWith tracer ("attempting to read " ++ show bufSize ++ " bytes...")
              unsafeReceiveN s buf bufSize >>= \case
                0 -> do
                  traceWith tracer "NO READ"
                  putMVar noReadVar ()
                n -> do
                  traceWith tracer ("read " ++ show n ++ " bytes")
                  when (fromIntegral n /= bufSize) (error "BBBBBB")
          )
  traceWith tracer "receiving t..."
  t <- fromIntegral <$> (
        maybe (putMVar noReadVar () >> return 0) return =<< receiveWord32 s
       )
  traceWith tracer "receiving l..."
  l <- fromIntegral <$> (
        maybe (putMVar noReadVar () >> return 0) return =<< receiveWord32 s
       )
  traceWith tracer "receiving oc..."
  oc <- unsafeDeserialize' <$> receiveBS s l
  traceWith tracer "done receiving"

  skpVar <- newCRef
              (forgetSignKeyKES . skWithoutPeriodKES)
              (SignKeyWithPeriodKES sk t)

  succeeded <- isEmptyMVar noReadVar
  if succeeded then
    return $ Just (skpVar, oc)
  else do
    -- We're not actually returning the key, so we must release it
    -- before exiting.
    releaseCRef skpVar
    return Nothing

receiveBS :: (MonadST m, MonadThrow m)
          => RawBearer m
          -> Int
          -> m BS.ByteString
receiveBS s size =
  allocaBytes size $ \buf -> do
    bytesRead <- unsafeReceiveN s buf (fromIntegral size)
    case bytesRead of
      0 -> return ""
      n -> packByteStringCStringLen (buf, fromIntegral n)

sendBS :: (MonadST m, MonadThrow m)
       => RawBearer m
       -> BS.ByteString
       -> m Int
sendBS s bs =
  allocaBytes (BS.length bs) $ \buf -> do
    unpackByteStringCStringLen bs $ \(bsbuf, size) -> do
      copyMem buf bsbuf (fromIntegral size)
    fromIntegral <$> send s (castPtr buf) (fromIntegral $ BS.length bs)

receiveWord32 :: (MonadThrow m, MonadST m)
              => RawBearer m
              -> m (Maybe Word32)
receiveWord32 s =
  receiveBS s 4 >>= \case
    "" -> return Nothing
    xs -> (return . Just . fromIntegral) (decode @Word32 $ LBS.fromStrict xs)

sendWord32 :: (MonadThrow m, MonadST m)
           => RawBearer m
           -> Word32
           -> m ()
sendWord32 s val =
  void $ sendBS s (LBS.toStrict $ encode val)

unsafeReceiveN :: Monad m => RawBearer m -> Ptr CChar -> CSize -> m CSize
unsafeReceiveN s buf bufSize = do
  n <- recv s (castPtr buf) (fromIntegral bufSize)
  if fromIntegral n == bufSize then
    return bufSize
  else if n == 0 then
    return 0
  else do
    n' <- unsafeReceiveN s (plusPtr buf (fromIntegral n)) (bufSize - fromIntegral n)
    if n' == 0 then
      return 0
    else
      return bufSize

encodeRecvResult :: RecvResult -> Word32
encodeRecvResult RecvOK = 0
encodeRecvResult RecvErrorKeyOutdated = 1
encodeRecvResult RecvErrorInvalidOpCert = 2
encodeRecvResult RecvErrorUnknown = 0xFFFF

decodeRecvResult :: Word32 -> RecvResult
decodeRecvResult 0 = RecvOK
decodeRecvResult 1 = RecvErrorKeyOutdated
decodeRecvResult 2 = RecvErrorInvalidOpCert
decodeRecvResult _ = RecvErrorUnknown
