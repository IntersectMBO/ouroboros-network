{-# OPTIONS_GHC -Wno-deprecations #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Cardano.KESAgent.Driver
where

import Foreign (Ptr, plusPtr)
import Foreign.C.Types (CSize, CChar)
import Foreign.Marshal.Alloc (mallocBytes, free)
import Foreign.Marshal.Utils (copyBytes)
import Network.TypedProtocol.Core
import Network.TypedProtocol.Driver
import Control.Monad (void, when)
import Data.Proxy
import Text.Printf
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Binary (encode, decode)
import Data.Typeable
import Control.Tracer (Tracer, traceWith)
import Control.Monad.Class.MonadThrow (MonadThrow, bracket)
import Control.Monad.Class.MonadMVar
import Control.Monad.Class.MonadST

import Cardano.Crypto.KES.Class
import Cardano.Crypto.DirectSerialise
import Cardano.Crypto.MonadMLock
          ( MonadMLock (..)
          , MonadUnmanagedMemory (..)
          , MonadByteStringMemory (..)
          , packByteStringCStringLen
          )
import Cardano.Binary

import Cardano.KESAgent.Protocol
import Cardano.KESAgent.OCert
import Cardano.KESAgent.RefCounting
import Cardano.KESAgent.DirectBearer

-- | Logging messages that the Driver may send
data DriverTrace
  = DriverSendingVersionID VersionIdentifier
  | DriverReceivingVersionID
  | DriverReceivedVersionID VersionIdentifier
  | DriverSendingKey
  | DriverSentKey
  | DriverReceivingKey
  | DriverReceivedKey
  | DriverConnectionClosed
  deriving (Show)

driver :: forall c m f t p
        . Crypto c
       => Typeable c
       => VersionedProtocol (KESProtocol m c)
       => KESSignAlgorithm m (KES c)
       => DirectDeserialise m (SignKeyKES (KES c))
       => DirectSerialise m (SignKeyKES (KES c))
       => MonadThrow m
       => MonadMVar m
       => MonadFail m
       => MonadUnmanagedMemory m
       => MonadByteStringMemory m
       => MonadST m
       => DirectBearer m
       -> Tracer m DriverTrace
       -> Driver (KESProtocol m c) () m
driver s tracer = Driver
  { sendMessage = \agency msg -> case (agency, msg) of
      (ServerAgency TokInitial, VersionMessage) -> do
        let VersionIdentifier v = versionIdentifier (Proxy @(KESProtocol m c))
        traceWith tracer (DriverSendingVersionID $ VersionIdentifier v)
        void $ sendBS s v
      (ServerAgency TokIdle, KeyMessage skpRef oc) -> do
        traceWith tracer DriverSendingKey
        withCRefValue skpRef $ \(SignKeyWithPeriodKES sk t) -> do
          directSerialise (\buf bufSize -> do
                n <- send s buf bufSize
                when (fromIntegral n /= bufSize) (error "AAAAA")
              ) sk
          let serializedOC = serialize' oc
          sendWord32 s (fromIntegral t)
          sendWord32 s (fromIntegral (BS.length serializedOC))
          sendBS s serializedOC
          traceWith tracer DriverSentKey
      (ServerAgency TokIdle, EndMessage) -> do
        return ()

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

        skpVar <- newCRef (forgetSignKeyKES . skWithoutPeriodKES) (SignKeyWithPeriodKES sk t)
        succeeded <- isEmptyMVar noReadVar
        if succeeded then do
          traceWith tracer DriverReceivedKey
          return (SomeMessage (KeyMessage skpVar oc), ())
        else do
          -- We're not actually returning the key, so we must release it
          -- before exiting.
          releaseCRef skpVar
          traceWith tracer DriverConnectionClosed
          return (SomeMessage EndMessage, ())

  , startDState = ()
  }

receiveBS :: (MonadST m, MonadThrow m, MonadUnmanagedMemory m, MonadByteStringMemory m)
          => DirectBearer m
          -> Int
          -> m BS.ByteString
receiveBS s size =
  allocaBytes size $ \buf -> do
    bytesRead <- unsafeReceiveN s buf (fromIntegral size)
    case bytesRead of
      0 -> return ""
      n -> packByteStringCStringLen (buf, fromIntegral n)

sendBS :: (MonadThrow m, MonadUnmanagedMemory m, MonadByteStringMemory m)
       => DirectBearer m
       -> BS.ByteString
       -> m Int
sendBS s bs =
  allocaBytes (BS.length bs) $ \buf -> do
    useByteStringAsCStringLen bs $ \(bsbuf, size) -> do
      copyMem buf bsbuf (fromIntegral size)
    fromIntegral <$> send s buf (fromIntegral $ BS.length bs)

receiveWord32 :: (MonadThrow m, MonadST m, MonadUnmanagedMemory m, MonadByteStringMemory m)
              => DirectBearer m
              -> m (Maybe Word32)
receiveWord32 s =
  receiveBS s 4 >>= \case
    "" -> return Nothing
    xs -> (return . Just . fromIntegral) (decode @Word32 $ LBS.fromStrict xs)

sendWord32 :: (MonadThrow m, MonadUnmanagedMemory m, MonadByteStringMemory m)
           => DirectBearer m
           -> Word32
           -> m ()
sendWord32 s val =
  void $ sendBS s (LBS.toStrict $ encode val)

unsafeReceiveN :: Monad m => DirectBearer m -> Ptr CChar -> CSize -> m CSize
unsafeReceiveN s buf bufSize = do
  n <- recv s buf bufSize
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
