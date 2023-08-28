{-# OPTIONS_GHC -Wno-deprecations #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.KESAgent.Serialization.RawUtil
where

import Cardano.KESAgent.KES.Crypto
import Cardano.KESAgent.KES.OCert
import Cardano.KESAgent.Protocols.VersionedProtocol
import Cardano.KESAgent.Protocols.RecvResult
import Cardano.KESAgent.Util.Pretty
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
import Data.Maybe ( fromMaybe )
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
           -> m ()
sendBundle s skpRef oc = do
  sendSKP s skpRef
  sendOC s oc

sendSKP :: ( MonadST m
              , MonadSTM m
              , MonadThrow m
              , KESAlgorithm k
              , DirectSerialise m (SignKeyKES k)
              )
           => RawBearer m
           -> CRef m (SignKeyWithPeriodKES k)
           -> m ()
sendSKP s skpRef = do
  withCRefValue skpRef $ \(SignKeyWithPeriodKES sk t) -> do
    directSerialise (\buf bufSize -> do
          n <- send s (castPtr buf) (fromIntegral bufSize)
          when (fromIntegral n /= bufSize) (error "AAAAA")
        ) sk
    sendWord32 s (fromIntegral t)

sendOC :: ( MonadST m
          , MonadThrow m
          , Crypto c
          , Typeable c
          )
       => RawBearer m
       -> OCert c
       -> m ()
sendOC s oc = do
  let serializedOC = serialize' oc
  sendWord32 s (fromIntegral (BS.length serializedOC))
  void $ sendBS s serializedOC

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
  skpMay <- receiveSKP s tracer
  case skpMay of
    Just skp -> do
      oc <- receiveOC s tracer
      return $ Just (skp, oc)
    Nothing ->
      return Nothing

receiveSKP :: ( MonadST m
                 , MonadSTM m
                 , MonadMVar m
                 , MonadThrow m
                 , KESAlgorithm k
                 , DirectDeserialise m (SignKeyKES k)
                 )
              => RawBearer m
              -> Tracer m String
              -> m (Maybe (CRef m (SignKeyWithPeriodKES k)))
receiveSKP s tracer = do
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
  skpVar <- newCRef
              (forgetSignKeyKES . skWithoutPeriodKES)
              (SignKeyWithPeriodKES sk t)

  succeeded <- isEmptyMVar noReadVar
  if succeeded then
    return $ Just skpVar
  else do
    -- We're not actually returning the key, so we must release it
    -- before exiting.
    releaseCRef skpVar
    return Nothing

receiveOC :: ( MonadST m
                 , MonadSTM m
                 , MonadMVar m
                 , MonadThrow m
                 , Crypto c
                 , Typeable c
                 , DirectDeserialise m (SignKeyKES (KES c))
                 )
              => RawBearer m
              -> Tracer m String
              -> m (OCert c)
receiveOC s tracer = do
  traceWith tracer "receiving l..."
  l <- fromIntegral . fromMaybe 0 <$> receiveWord32 s
  traceWith tracer "receiving oc..."
  oc <- unsafeDeserialize' <$> receiveBS s l
  traceWith tracer "done receiving"
  return oc

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

sendRecvResult :: ( MonadST m
                 , MonadThrow m
                 )
              => RawBearer m -> RecvResult -> m ()
sendRecvResult s r =
  sendWord32 s (encodeRecvResult r)

receiveRecvResult :: ( MonadST m
                    , MonadThrow m
                    )
                 => RawBearer m -> m RecvResult
receiveRecvResult s =
  maybe RecvErrorUnknown decodeRecvResult <$> receiveWord32 s

encodeRecvResult :: RecvResult -> Word32
encodeRecvResult RecvOK = 0
encodeRecvResult RecvErrorKeyOutdated = 1
encodeRecvResult RecvErrorInvalidOpCert = 2
encodeRecvResult RecvErrorNoKey = 3
encodeRecvResult RecvErrorUnknown = 0xFFFF

decodeRecvResult :: Word32 -> RecvResult
decodeRecvResult 0 = RecvOK
decodeRecvResult 1 = RecvErrorKeyOutdated
decodeRecvResult 2 = RecvErrorInvalidOpCert
decodeRecvResult 3 = RecvErrorNoKey
decodeRecvResult _ = RecvErrorUnknown
