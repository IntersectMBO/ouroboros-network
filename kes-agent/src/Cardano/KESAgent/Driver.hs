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
module Cardano.KESAgent.Driver
  where

import Cardano.KESAgent.OCert
import Cardano.KESAgent.Pretty
import Cardano.KESAgent.Protocol
import Cardano.KESAgent.RefCounting

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
import Data.Functor.Contravariant (contramap)
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
data DriverTrace
  = DriverSendingVersionID VersionIdentifier
  | DriverReceivingVersionID
  | DriverReceivedVersionID VersionIdentifier
  | DriverSendingKey Word64
  | DriverSentKey Word64
  | DriverReceivingKey
  | DriverReceivedKey Word64
  | DriverConfirmingKey
  | DriverConfirmedKey
  | DriverDecliningKey
  | DriverDeclinedKey
  | DriverConnectionClosed
  | DriverCRefEvent CRefEvent
  deriving (Show)

instance Pretty DriverTrace where
  pretty x = drop (strLength "Driver") (show x)

driver :: forall c m f t p
        . Crypto c
       => Typeable c
       => VersionedProtocol (KESProtocol m c)
       => KESAlgorithm (KES c)
       => DirectDeserialise m (SignKeyKES (KES c))
       => DirectSerialise m (SignKeyKES (KES c))
       => MonadThrow m
       => MonadSTM m
       => MonadMVar m
       => MonadFail m
       => MonadST m
       => RawBearer m
       -> Tracer m DriverTrace
       -> Driver (KESProtocol m c) () m
driver s tracer = Driver
  { sendMessage = \agency msg -> case (agency, msg) of
      (ServerAgency TokInitial, VersionMessage) -> do
        let VersionIdentifier v = versionIdentifier (Proxy @(KESProtocol m c))
        traceWith tracer (DriverSendingVersionID $ VersionIdentifier v)
        void $ sendBS s v
      (ServerAgency TokIdle, KeyMessage skpRef oc) -> do
        traceWith tracer $ DriverSendingKey (ocertN oc)
        withCRefValue skpRef $ \(SignKeyWithPeriodKES sk t) -> do
          directSerialise (\buf bufSize -> do
                n <- send s (castPtr buf) (fromIntegral bufSize)
                when (fromIntegral n /= bufSize) (error "AAAAA")
              ) sk
          let serializedOC = serialize' oc
          sendWord32 s (fromIntegral t)
          sendWord32 s (fromIntegral (BS.length serializedOC))
          sendBS s serializedOC
          traceWith tracer $ DriverSentKey (ocertN oc)
      (ServerAgency TokIdle, EndMessage) -> do
        return ()
      (ClientAgency TokWaitForConfirmation, RecvErrorMessage) -> do
        traceWith tracer DriverDecliningKey
        void $ sendWord32 s 0x00000000
      (ClientAgency TokWaitForConfirmation, ConfirmMessage) -> do
        traceWith tracer DriverConfirmingKey
        void $ sendWord32 s 0x12345678

  , recvMessage = \agency () -> case agency of
      (ServerAgency TokInitial) -> do
        traceWith tracer DriverReceivingVersionID
        v <- VersionIdentifier <$> receiveBS s versionIdentifierLength
        traceWith tracer $ DriverReceivedVersionID v
        let expectedV = versionIdentifier (Proxy @(KESProtocol m c))
        if v == expectedV then
          return (SomeMessage VersionMessage, ())
        else
          fail $ "DRIVER: Invalid version, expected " ++ show expectedV ++ ", but got " ++ show v
      (ServerAgency TokIdle) -> do
        traceWith tracer DriverReceivingKey
        noReadVar <- newEmptyMVar
        sk <- directDeserialise
                (\buf bufSize -> unsafeReceiveN s buf bufSize >>= \case
                    0 -> putMVar noReadVar ()
                    n -> when (fromIntegral n /= bufSize) (error "BBBBBB")
                )
        t <- fromIntegral <$> (
              maybe (putMVar noReadVar () >> return 0) return =<< receiveWord32 s
             )
        l <- fromIntegral <$> (
              maybe (putMVar noReadVar () >> return 0) return =<< receiveWord32 s
             )
        oc <- unsafeDeserialize' <$> receiveBS s l

        skpVar <- newCRefWith
                    (contramap DriverCRefEvent tracer)
                    (forgetSignKeyKES . skWithoutPeriodKES) (SignKeyWithPeriodKES sk t)
        succeeded <- isEmptyMVar noReadVar
        if succeeded then do
          traceWith tracer $ DriverReceivedKey (ocertN oc)
          return (SomeMessage (KeyMessage skpVar oc), ())
        else do
          -- We're not actually returning the key, so we must release it
          -- before exiting.
          releaseCRef skpVar
          traceWith tracer DriverConnectionClosed
          return (SomeMessage EndMessage, ())
      (ClientAgency TokWaitForConfirmation) -> do
        nonce <- receiveWord32 s
        if nonce == Just 0x12345678 then do
          traceWith tracer DriverConfirmedKey
          return (SomeMessage ConfirmMessage, ())
        else do
          traceWith tracer DriverDeclinedKey
          return (SomeMessage RecvErrorMessage, ())

  , startDState = ()
  }

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
