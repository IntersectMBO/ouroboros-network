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
{-# LANGUAGE DeriveFunctor #-}

module Cardano.KESAgent.Serialization.RawUtil
where

import Cardano.KESAgent.Protocols.VersionedProtocol
import Cardano.KESAgent.Protocols.RecvResult
import Cardano.KESAgent.Util.RefCounting

import Cardano.Crypto.DirectSerialise
import Cardano.Crypto.KES.Class
import Cardano.Crypto.Libsodium.Memory
  ( allocaBytes
  , copyMem
  , packByteStringCStringLen
  , unpackByteStringCStringLen
  )

import Ouroboros.Network.RawBearer

import Control.Applicative
import Control.Monad ( void, when )
import Control.Monad.Trans
import Control.Monad.Class.MonadMVar
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow ( MonadThrow, Exception )
import Control.Tracer ( Tracer, traceWith )
import Data.Binary ( decode, encode )
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Proxy
import Data.Typeable
import Data.Word
import Data.Int
import Foreign ( Ptr, castPtr, plusPtr )
import Foreign.C.Types ( CChar, CSize )
import Data.Time ( UTCTime )
import Data.Time.Clock.POSIX ( utcTimeToPOSIXSeconds, posixSecondsToUTCTime )

data ReadResult a
  = ReadOK a
  | ReadMalformed !String
  | ReadVersionMismatch !VersionIdentifier !VersionIdentifier
  | ReadEOF
  deriving (Show, Eq, Functor)

instance (Show a, Typeable a) => Exception (ReadResult a) where

newtype ReadResultT m a = ReadResultT { runReadResultT :: m (ReadResult a) }
  deriving (Functor)

readResultT :: Applicative m => ReadResult a -> ReadResultT m a
readResultT r = ReadResultT (pure r)

instance (Functor m, Applicative m, Monad m) => Applicative (ReadResultT m) where
  pure = ReadResultT . pure . ReadOK

  liftA2 f a b = ReadResultT $ do
    rA <- runReadResultT a
    case rA of
      ReadOK valA -> do
        rB <- runReadResultT b
        case rB of
          ReadOK valB -> do
            pure . ReadOK $ f valA valB
          x -> pure $ undefined <$ x
      x -> pure $ undefined <$ x

instance Monad m => Monad (ReadResultT m) where
  ReadResultT aM >>= f =
    ReadResultT $ do
      r <- aM
      case r of
        ReadOK x -> runReadResultT (f x)
        _ -> pure $ undefined <$ r

instance MonadTrans ReadResultT where
  lift = ReadResultT . fmap ReadOK

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

checkVersion :: forall protocol m.
                ( VersionedProtocol protocol
                , MonadST m
                , MonadThrow m
                , MonadFail m
                )
             => Proxy protocol
             -> RawBearer m
             -> Tracer m VersionIdentifier
             -> m (ReadResult ())
checkVersion p s tracer = runReadResultT $ do
  v <- VersionIdentifier <$> ReadResultT (receiveBS s versionIdentifierLength)
  lift $ traceWith tracer v
  let expectedV = versionIdentifier p
  if v == expectedV then
    return ()
  else
    readResultT $ ReadVersionMismatch expectedV v

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

receiveSK :: ( MonadST m
             , MonadSTM m
             , MonadMVar m
             , MonadThrow m
             , KESAlgorithm k
             , DirectDeserialise m (SignKeyKES k)
             )
           => RawBearer m
           -> Tracer m String
           -> m (ReadResult (SignKeyKES k))
receiveSK s tracer = do
  errVar <- newEmptyMVar
  traceWith tracer "waiting for sign key bytes..."
  sk <- directDeserialise
    (\buf bufSize -> do
        traceWith tracer ("attempting to read " ++ show bufSize ++ " bytes...")
        unsafeReceiveN s buf bufSize >>= \case
          ReadOK n -> do
            traceWith tracer ("read " ++ show n ++ " bytes")
            when (fromIntegral n /= bufSize) (error "BBBBBB")
          x ->  do
            traceWith tracer "NO READ"
            _ <- tryTakeMVar errVar
            putMVar errVar x
    )
  err <- tryTakeMVar errVar
  case err of
    Nothing -> do
      return $ ReadOK sk
    Just (ReadOK _) -> do
      return $ ReadOK sk
    Just result -> do
      forgetSignKeyKES sk
      return $ undefined <$ result

receiveSKP :: ( MonadST m
                 , MonadSTM m
                 , MonadMVar m
                 , MonadThrow m
                 , KESAlgorithm k
                 , DirectDeserialise m (SignKeyKES k)
                 )
              => RawBearer m
              -> Tracer m String
              -> m (ReadResult (CRef m (SignKeyWithPeriodKES k)))
receiveSKP s tracer = runReadResultT $ do
  lift $ traceWith tracer "waiting for sign key bytes..."
  sk <- ReadResultT $ receiveSK s tracer
  lift $ traceWith tracer "receiving t..."
  t <- fromIntegral <$> ReadResultT (receiveWord32 s)
  lift $ newCRef
    (forgetSignKeyKES . skWithoutPeriodKES)
    (SignKeyWithPeriodKES sk t)

receiveBS :: (MonadST m, MonadThrow m)
          => RawBearer m
          -> Int
          -> m (ReadResult BS.ByteString)
receiveBS s size =
  allocaBytes size $ \buf -> runReadResultT $ do
    bytesRead <- ReadResultT $ unsafeReceiveN s buf (fromIntegral size)
    if bytesRead /= fromIntegral size then
      readResultT $ ReadMalformed $ "ByteString of length " ++ show size
    else
      lift $ packByteStringCStringLen (buf, fromIntegral bytesRead)

sendBS :: (MonadST m, MonadThrow m)
       => RawBearer m
       -> BS.ByteString
       -> m Int
sendBS s bs =
  allocaBytes (BS.length bs) $ \buf -> do
    unpackByteStringCStringLen bs $ \(bsbuf, size) -> do
      copyMem buf bsbuf (fromIntegral size)
    fromIntegral <$> send s (castPtr buf) (fromIntegral $ BS.length bs)

receiveWord8 :: (MonadThrow m, MonadST m)
              => RawBearer m
              -> m (ReadResult Word8)
receiveWord8 s = runReadResultT $ do
  buf <- ReadResultT $ receiveBS s 1
  let decoded = decode @Word8 $ LBS.fromStrict buf
  return decoded

receiveWord16 :: (MonadThrow m, MonadST m)
              => RawBearer m
              -> m (ReadResult Word16)
receiveWord16 s = runReadResultT $ do
  buf <- ReadResultT $ receiveBS s 2
  let decoded = decode @Word16 $ LBS.fromStrict buf
  return decoded

receiveWord32 :: (MonadThrow m, MonadST m)
              => RawBearer m
              -> m (ReadResult Word32)
receiveWord32 s = runReadResultT $ do
  buf <- ReadResultT $ receiveBS s 4
  let decoded = decode @Word32 $ LBS.fromStrict buf
  return decoded

receiveWord64 :: (MonadThrow m, MonadST m)
              => RawBearer m
              -> m (ReadResult Word64)
receiveWord64 s = runReadResultT $ do
  buf <- ReadResultT $ receiveBS s 8
  let decoded = decode @Word64 $ LBS.fromStrict buf
  return decoded

receiveInt8 :: (MonadThrow m, MonadST m)
              => RawBearer m
              -> m (ReadResult Int8)
receiveInt8 s = runReadResultT $ do
  buf <- ReadResultT $ receiveBS s 1
  let decoded = decode @Int8 $ LBS.fromStrict buf
  return decoded

receiveInt16 :: (MonadThrow m, MonadST m)
              => RawBearer m
              -> m (ReadResult Int16)
receiveInt16 s = runReadResultT $ do
  buf <- ReadResultT $ receiveBS s 2
  let decoded = decode @Int16 $ LBS.fromStrict buf
  return decoded

receiveInt32 :: (MonadThrow m, MonadST m)
              => RawBearer m
              -> m (ReadResult Int32)
receiveInt32 s = runReadResultT $ do
  buf <- ReadResultT $ receiveBS s 4
  let decoded = decode @Int32 $ LBS.fromStrict buf
  return decoded

receiveInt64 :: (MonadThrow m, MonadST m)
              => RawBearer m
              -> m (ReadResult Int64)
receiveInt64 s = runReadResultT $ do
  buf <- ReadResultT $ receiveBS s 8
  let decoded = decode @Int64 $ LBS.fromStrict buf
  return decoded


sendWord8 :: (MonadThrow m, MonadST m)
           => RawBearer m
           -> Word8
           -> m ()
sendWord8 s val =
  void $ sendBS s (LBS.toStrict $ encode val)

sendWord16 :: (MonadThrow m, MonadST m)
           => RawBearer m
           -> Word16
           -> m ()
sendWord16 s val =
  void $ sendBS s (LBS.toStrict $ encode val)

sendWord32 :: (MonadThrow m, MonadST m)
           => RawBearer m
           -> Word32
           -> m ()
sendWord32 s val =
  void $ sendBS s (LBS.toStrict $ encode val)

sendWord64 :: (MonadThrow m, MonadST m)
           => RawBearer m
           -> Word64
           -> m ()
sendWord64 s val =
  void $ sendBS s (LBS.toStrict $ encode val)

sendInt8 :: (MonadThrow m, MonadST m)
           => RawBearer m
           -> Int8
           -> m ()
sendInt8 s val =
  void $ sendBS s (LBS.toStrict $ encode val)

sendInt16 :: (MonadThrow m, MonadST m)
           => RawBearer m
           -> Int16
           -> m ()
sendInt16 s val =
  void $ sendBS s (LBS.toStrict $ encode val)

sendInt32 :: (MonadThrow m, MonadST m)
           => RawBearer m
           -> Int32
           -> m ()
sendInt32 s val =
  void $ sendBS s (LBS.toStrict $ encode val)

sendInt64 :: (MonadThrow m, MonadST m)
           => RawBearer m
           -> Int64
           -> m ()
sendInt64 s val =
  void $ sendBS s (LBS.toStrict $ encode val)

sendUTCTime :: (MonadThrow m, MonadST m)
            => RawBearer m
            -> UTCTime
            -> m ()
sendUTCTime s utc =
  sendInt64 s $ floor . utcTimeToPOSIXSeconds $ utc

receiveUTCTime :: (MonadThrow m, MonadST m)
            => RawBearer m
            -> m (ReadResult UTCTime)
receiveUTCTime s = runReadResultT $ do
  posix <- ReadResultT $ receiveInt64 s
  return $ posixSecondsToUTCTime . fromIntegral $ posix

sendEnum :: forall m a.
               ( MonadST m
               , MonadThrow m
               , Enum a
               , Bounded a
               )
            => RawBearer m
            -> a
            -> m ()
sendEnum s =
  sendWord32 s . fromIntegral . fromEnum

receiveEnum :: forall m a.
               ( MonadST m
               , MonadThrow m
               , Enum a
               , Bounded a
               )
            => String
            -> RawBearer m
            -> m (ReadResult a)
receiveEnum label s = runReadResultT $ do
  w <- fromIntegral <$> ReadResultT (receiveWord32 s)
  if w > fromEnum (maxBound :: a) then
    readResultT (ReadMalformed label)
  else
    return $ toEnum w


unsafeReceiveN :: Monad m => RawBearer m -> Ptr CChar -> CSize -> m (ReadResult CSize)
unsafeReceiveN s buf bufSize = do
  n <- recv s (castPtr buf) (fromIntegral bufSize)
  if fromIntegral n == bufSize then
    return (ReadOK bufSize)
  else if n == 0 then
    return ReadEOF
  else runReadResultT $ do
    n' <- ReadResultT $ unsafeReceiveN s (plusPtr buf (fromIntegral n)) (bufSize - fromIntegral n)
    if n' == 0 then
      readResultT ReadEOF
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
                 => RawBearer m -> m (ReadResult RecvResult)
receiveRecvResult s = runReadResultT $ do
  w <- ReadResultT $ receiveWord32 s
  return $ decodeRecvResult w

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
