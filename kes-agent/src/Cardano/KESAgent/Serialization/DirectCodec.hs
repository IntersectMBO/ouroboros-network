{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}

-- | A codec used for direct serialization (sending and receiving data through
-- file descriptors without requiring any intermediate memory buffers).
module Cardano.KESAgent.Serialization.DirectCodec
where

import Cardano.KESAgent.KES.Bundle
import Cardano.KESAgent.KES.Crypto
import Cardano.KESAgent.KES.OCert
import Cardano.KESAgent.Protocols.RecvResult
import Cardano.KESAgent.Protocols.VersionedProtocol
import Cardano.KESAgent.Serialization.RawUtil
import Cardano.KESAgent.Util.RefCounting

import Cardano.Crypto.DSIGN.Class
import Cardano.Crypto.DirectSerialise
import Cardano.Crypto.Hash.Class
import Cardano.Crypto.KES.Class
import Cardano.Crypto.KES.CompactSingle
import Cardano.Crypto.KES.CompactSum
import Cardano.Crypto.KES.Mock
import Cardano.Crypto.KES.Single
import Cardano.Crypto.KES.Sum
import Cardano.Crypto.Libsodium.Hash.Class
import Ouroboros.Network.RawBearer (RawBearer (..))

import Control.Concurrent.Class.MonadMVar
import Control.Monad
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow
import Control.Monad.Reader
import qualified Data.ByteString as BS
import Data.Int
import Data.Kind
import Data.SerDoc.Class
import Data.SerDoc.Info
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time
import Data.Typeable
import Data.Word
import Foreign.Ptr (castPtr)
import GHC.TypeLits (KnownNat, type (*), type (+))

data DirectCodec (m :: Type -> Type)

instance Codec (DirectCodec m) where
  type MonadEncode (DirectCodec m) = ReaderT (RawBearer m) m
  type MonadDecode (DirectCodec m) = ReaderT (RawBearer m) (ReadResultT m)

sendItem ::
  forall m a.
  ( Serializable (DirectCodec m) a
  , Monad m
  ) =>
  RawBearer m ->
  a ->
  m ()
sendItem s x =
  runReaderT (encode (Proxy @(DirectCodec m)) x) s

receiveItem ::
  forall m a.
  ( Serializable (DirectCodec m) a
  , Monad m
  ) =>
  RawBearer m ->
  ReadResultT m a
receiveItem s =
  runReaderT (decode (Proxy @(DirectCodec m))) s

-- * Instances for basic types

-- ** Unit

instance HasInfo (DirectCodec m) () where
  info _ _ = basicField "void" $ FixedSize 0

instance Monad m => Serializable (DirectCodec m) () where
  encode _ () = return ()
  decode _ = return ()

-- ** Int types

instance HasInfo (DirectCodec m) Int8 where
  info _ _ = basicField "int8" $ FixedSize 1

encodeWith f x = do
  s <- ask
  lift $ f s x

decodeWith f = do
  s <- ask
  lift . ReadResultT $ f s

instance (MonadThrow m, MonadST m) => Serializable (DirectCodec m) Int8 where
  encode _ = encodeWith sendInt8
  decode _ = decodeWith receiveInt8

instance HasInfo (DirectCodec m) Int16 where
  info _ _ = basicField "int16BE" $ FixedSize 2

instance (MonadThrow m, MonadST m) => Serializable (DirectCodec m) Int16 where
  encode _ = encodeWith sendInt16
  decode _ = decodeWith receiveInt16

instance HasInfo (DirectCodec m) Int32 where
  info _ _ = basicField "int32BE" $ FixedSize 4

instance (MonadThrow m, MonadST m) => Serializable (DirectCodec m) Int32 where
  encode _ = encodeWith sendInt32
  decode _ = decodeWith receiveInt32

instance HasInfo (DirectCodec m) Int64 where
  info _ _ = basicField "int64BE" $ FixedSize 8

instance (MonadThrow m, MonadST m) => Serializable (DirectCodec m) Int64 where
  encode _ = encodeWith sendInt64
  decode _ = decodeWith receiveInt64

instance HasInfo (DirectCodec m) Word8 where
  info _ _ = basicField "word8" $ FixedSize 1

instance (MonadThrow m, MonadST m) => Serializable (DirectCodec m) Word8 where
  encode _ = encodeWith sendWord8
  decode _ = decodeWith receiveWord8

instance HasInfo (DirectCodec m) Word16 where
  info _ _ = basicField "word16BE" $ FixedSize 2

instance (MonadThrow m, MonadST m) => Serializable (DirectCodec m) Word16 where
  encode _ = encodeWith sendWord16
  decode _ = decodeWith receiveWord16

instance HasInfo (DirectCodec m) Word32 where
  info _ _ = basicField "word32BE" $ FixedSize 4

instance (MonadThrow m, MonadST m) => Serializable (DirectCodec m) Word32 where
  encode _ = encodeWith sendWord32
  decode _ = decodeWith receiveWord32

instance HasInfo (DirectCodec m) Word64 where
  info _ _ = basicField "word64BE" $ FixedSize 8

instance (MonadThrow m, MonadST m) => Serializable (DirectCodec m) Word64 where
  encode _ = encodeWith sendWord64
  decode _ = decodeWith receiveWord64

-- ** 'UTCTime'

-- Note that the default precision is 1 second. We use 'UTCTime' directly when
-- reporting times to control clients; timestamps on key bundles have their own
-- 'Timestamp' type, which serializes to millisecond granularity.

instance HasInfo (DirectCodec m) UTCTime where
  info codec _ =
    aliasField "POSIXSeconds" $
      info codec (Proxy @Int64)

instance (MonadThrow m, MonadST m) => Serializable (DirectCodec m) UTCTime where
  encode _ = encodeWith sendUTCTime
  decode _ = decodeWith receiveUTCTime

-- ** 'Bool'

instance HasInfo (DirectCodec m) Bool where
  info _ _ = basicField "Bool" (FixedSize 1)

instance (MonadThrow m, MonadST m) => Serializable (DirectCodec m) Bool where
  encode _ = encodeWith (\s -> sendWord8 s . fromIntegral . fromEnum)
  decode _ =
    decodeWith
      ( \s -> runReadResultT $ do
          ReadResultT (receiveWord8 s) >>= \case
            0 -> return False
            1 -> return True
            _ -> ReadResultT . return $ ReadMalformed "Bool"
      )

instance HasInfo (DirectCodec m) a => HasInfo (DirectCodec m) (Maybe a) where
  info codec _ =
    compoundField
      ("Maybe " ++ shortFieldType (info codec (Proxy @a)))
      [ ("isJust", info codec (Proxy @Bool))
      ,
        ( "value"
        , choiceField
            (IndexField "isJust")
            [ info codec (Proxy @())
            , info codec (Proxy @a)
            ]
        )
      ]

instance
  ( Serializable (DirectCodec m) a
  , Serializable (DirectCodec m) Bool
  , Monad m
  ) =>
  Serializable (DirectCodec m) (Maybe a)
  where
  encode codec Nothing =
    encode codec False
  encode codec (Just x) = do
    encode codec True
    encode codec x

  decode codec = do
    just <- decode codec
    if just
      then do
        Just <$> decode codec
      else
        return Nothing

-- ** Helpers for known-sized binary data

sizedInfo :: Int -> FieldInfo a
sizedInfo n =
  basicField "bytes" $ FixedSize n

encodeSized ::
  ( MonadST m
  , MonadThrow m
  ) =>
  Word ->
  ByteString ->
  ReaderT (RawBearer m) m ()
encodeSized expectedLen val = do
  s <- ask
  actualLen <- lift $ fromIntegral <$> sendBS s val
  unless (actualLen == expectedLen) (error "Length mismatch")

decodeSized ::
  ( MonadST m
  , MonadThrow m
  ) =>
  Int ->
  ReaderT (RawBearer m) (ReadResultT m) ByteString
decodeSized n = do
  s <- ask
  lift $ ReadResultT (receiveBS s (fromIntegral n))

-- ** 'ByteString'

instance HasInfo (DirectCodec m) ByteString where
  info codec _ =
    compoundField
      "ByteString"
      [ ("length", info codec (Proxy @Word32))
      , ("data", basicField "bytes" $ VarSize "length")
      ]
instance (MonadThrow m, MonadST m) => Serializable (DirectCodec m) ByteString where
  encode _ val = do
    s <- ask
    let len = BS.length val
    lift $ sendWord32 s (fromIntegral len)
    actualLen <- lift (sendBS s val)
    unless (actualLen == len) (error "Length mismatch")

  decode _ = do
    s <- ask
    lift $ do
      len <- ReadResultT (receiveWord32 s)
      ReadResultT (receiveBS s (fromIntegral len))

-- ** 'Text'

instance HasInfo (DirectCodec m) Text where
  info codec _ =
    compoundField
      "UTF8"
      [ ("length", info codec (Proxy @Word32))
      , ("data", basicField "bytes" $ VarSize "length")
      ]
instance (MonadThrow m, MonadST m) => Serializable (DirectCodec m) Text where
  encode _ = encodeWith $ \s val -> do
    let len = BS.length (encodeUtf8 val)
    sendWord32 s (fromIntegral len)
    actualLen <- sendBS s (encodeUtf8 val)
    unless (actualLen == len) (error "Length mismatch")

  decode _ = decodeWith $ \s -> runReadResultT $ do
    len <- ReadResultT (receiveWord32 s)
    decodeUtf8 <$> ReadResultT (receiveBS s (fromIntegral len))

-- ** Lists

instance HasInfo (DirectCodec m) a => HasInfo (DirectCodec m) [a] where
  info codec _ =
    compoundField
      ("[" ++ fieldType (info codec (Proxy @a)) ++ "]")
      [ ("listLength", info codec (Proxy @Word32))
      ,
        ( "elems"
        , listField
            (VarSize "listLength")
            (info codec (Proxy @a))
        )
      ]
instance (MonadThrow m, MonadST m, Serializable (DirectCodec m) a) => Serializable (DirectCodec m) [a] where
  encode codec val = do
    let len = fromIntegral (length val) :: Word32
    encode codec len
    mapM_ (encode codec) val

  decode codec = do
    len :: Word32 <- decode codec
    replicateM (fromIntegral len) (decode codec)

-- ** 'SodiumHashAlgorithm'

instance
  ( SodiumHashAlgorithm h
  , HashAlgorithm h
  ) =>
  HasInfo (DirectCodec m) (Hash h a)
  where
  info _ _ =
    aliasField ("Hash<" ++ hashAlgorithmName (Proxy @h) ++ ">") $
      basicField
        "bytes"
        (FixedSize . fromIntegral $ sizeHash (Proxy @h))

-- ** DSIGN

instance DSIGNAlgorithm dsign => HasInfo (DirectCodec m) (VerKeyDSIGN dsign) where
  info _ _ =
    aliasField ("VerKeyDSIGN " ++ algorithmNameDSIGN (Proxy @dsign)) $
      basicField
        "bytes"
        (FixedSize . fromIntegral $ sizeVerKeyDSIGN (Proxy @dsign))

instance
  ( MonadST m
  , MonadSTM m
  , MonadThrow m
  , DSIGNAlgorithm dsign
  ) =>
  Serializable (DirectCodec m) (VerKeyDSIGN dsign)
  where
  encode codec val =
    encodeSized (sizeVerKeyDSIGN (Proxy @dsign)) (rawSerialiseVerKeyDSIGN val)

  decode codec = do
    raw <- decodeSized (fromIntegral $ sizeVerKeyDSIGN (Proxy @dsign))
    let deser = rawDeserialiseVerKeyDSIGN raw
    case deser of
      Nothing -> lift . ReadResultT . return $ ReadMalformed "Invalid serialised VerKeyDSIGN"
      Just vk -> return vk

instance
  DSIGNAlgorithm dsign =>
  HasInfo (DirectCodec m) (SignKeyDSIGN dsign)
  where
  info _ _ =
    aliasField
      ("SignKeyDSIGN<" ++ algorithmNameDSIGN (Proxy @dsign) ++ ">")
      $ basicField
        "bytes"
        (FixedSize . fromIntegral $ sizeSignKeyDSIGN (Proxy @dsign))

instance
  ( MonadST m
  , MonadSTM m
  , MonadThrow m
  , DSIGNAlgorithm dsign
  ) =>
  Serializable (DirectCodec m) (SignKeyDSIGN dsign)
  where
  encode codec val =
    encodeSized (fromIntegral $ sizeSignKeyDSIGN (Proxy @dsign)) (rawSerialiseSignKeyDSIGN val)

  decode codec = do
    raw <- decodeSized (fromIntegral $ sizeSignKeyDSIGN (Proxy @dsign))
    let deser = rawDeserialiseSignKeyDSIGN raw
    case deser of
      Nothing -> lift . ReadResultT . return $ ReadMalformed "Invalid serialised SignKeyDSIGN"
      Just sk -> return sk

instance HasInfo codec (SigDSIGN d) => HasInfo codec (SignedDSIGN d a) where
  info codec _ =
    info codec (Proxy @(SigDSIGN d))

instance
  ( Monad m
  , Serializable (DirectCodec m) (SigDSIGN d)
  ) =>
  Serializable (DirectCodec m) (SignedDSIGN d a)
  where
  encode codec (SignedDSIGN sig) = encode codec sig
  decode codec = SignedDSIGN <$> decode codec

-- ** KES

-- We use the same actual ser/deser code for all KES algorithms, but the
-- 'HasInfo' implementations are different, in order to reflect the actual
-- data structures.

instance
  ( MonadSTM m
  , MonadThrow m
  , MonadST m
  , DirectSerialise (SignKeyKES kes)
  , DirectDeserialise (SignKeyKES kes)
  , KESAlgorithm kes
  ) =>
  Serializable (DirectCodec m) (SignKeyKES kes)
  where
  encode codec val = do
    s <- ask
    lift $
      directSerialise
        ( \buf bufSize -> do
            n <- send s (castPtr buf) (fromIntegral bufSize)
            when (fromIntegral n /= bufSize) (error "AAAAA")
        )
        val

  decode codec = do
    s <- ask
    lift . ReadResultT $ do
      sk <-
        directDeserialise
          ( \buf bufSize -> do
              unsafeReceiveN s buf bufSize >>= \case
                ReadOK n -> do
                  when
                    (fromIntegral n /= bufSize)
                    (throwIO (ReadMalformed "Incorrect number of key bytes" :: ReadResult ()))
                x -> do
                  throwIO x
          )
      return $ ReadOK sk

instance
  ( MonadST m
  , MonadSTM m
  , MonadST m
  , MonadThrow m
  , KESAlgorithm kes
  ) =>
  Serializable (DirectCodec m) (VerKeyKES kes)
  where
  encode codec val =
    encodeSized (sizeVerKeyKES (Proxy @kes)) (rawSerialiseVerKeyKES val)

  decode codec = do
    raw <- decodeSized (fromIntegral $ sizeVerKeyKES (Proxy @kes))
    let deser = rawDeserialiseVerKeyKES raw
    case deser of
      Nothing -> lift . ReadResultT . return $ ReadMalformed "Invalid serialised VerKeyKES"
      Just vk -> return vk

-- *** 'MockKES'

instance
  ( KnownNat t
  , KESAlgorithm (MockKES t)
  , HasInfo (DirectCodec m) (SignKeyKES (MockKES t))
  ) =>
  HasInfo (DirectCodec m) (VerKeyKES (MockKES t))
  where
  info codec _ =
    aliasField
      ("VerKeyKES<" ++ algorithmNameKES (Proxy @(MockKES t)) ++ ">")
      (info codec (Proxy @Word64))

instance
  ( KnownNat t
  , KESAlgorithm (MockKES t)
  ) =>
  HasInfo (DirectCodec m) (SignKeyKES (MockKES t))
  where
  info codec _ =
    compoundField
      ("SignKeyKES<" ++ algorithmNameKES (Proxy @(MockKES t)) ++ ">")
      [ ("verKey", info codec (Proxy @(VerKeyKES (MockKES t))))
      , ("period", info codec (Proxy @Word64))
      ]

-- *** 'SingleKES'

instance
  ( DSIGNAlgorithm dsign
  , KESAlgorithm (SingleKES dsign)
  ) =>
  HasInfo (DirectCodec m) (VerKeyKES (SingleKES dsign))
  where
  info codec _ =
    aliasField
      ("VerKeyKES<" ++ algorithmNameKES (Proxy @(SingleKES dsign)) ++ ">")
      (info codec (Proxy @(VerKeyDSIGN dsign)))

instance
  ( DSIGNAlgorithm dsign
  , KESAlgorithm (SingleKES dsign)
  , HasInfo (DirectCodec m) (SignKeyDSIGN dsign)
  ) =>
  HasInfo (DirectCodec m) (SignKeyKES (SingleKES dsign))
  where
  info codec _ =
    aliasField
      ("SignKeyKES<" ++ algorithmNameKES (Proxy @(SingleKES dsign)) ++ ">")
      (info codec (Proxy @(SignKeyDSIGN dsign)))

-- *** 'SumKES'

instance
  ( HashAlgorithm h
  , SodiumHashAlgorithm h
  , KESAlgorithm kes
  , KESAlgorithm (SumKES h kes)
  ) =>
  HasInfo (DirectCodec m) (VerKeyKES (SumKES h kes))
  where
  info codec _ =
    aliasField
      ("VerKeyKES<" ++ algorithmNameKES (Proxy @(SumKES h kes)) ++ ">")
      (info codec (Proxy @(Hash h (VerKeyKES kes, VerKeyKES kes))))

instance
  ( SodiumHashAlgorithm h
  , KESAlgorithm kes
  , HasInfo (DirectCodec m) (SignKeyKES kes)
  , HasInfo (DirectCodec m) (VerKeyKES kes)
  , SizeHash h ~ SeedSizeKES kes
  , KnownNat (SizeSignKeyKES kes)
  , KnownNat (SizeVerKeyKES kes)
  , KnownNat (SeedSizeKES kes)
  , KnownNat ((SizeSignKeyKES kes + SeedSizeKES kes) + (2 * SizeVerKeyKES kes))
  , KnownNat (SizeSigKES kes + (SizeVerKeyKES kes * 2))
  , forall a. HasInfo (DirectCodec m) (Hash h a)
  ) =>
  HasInfo (DirectCodec m) (SignKeyKES (SumKES h kes))
  where
  info codec _ =
    compoundField
      ("SignKeyKES<" ++ algorithmNameKES (Proxy @(SumKES h kes)) ++ ">")
      [ ("sk", info codec (Proxy @(SignKeyKES kes)))
      , ("seed", basicField "bytes" (FixedSize (fromIntegral $ seedSizeKES (Proxy @kes))))
      , ("vk0", info codec (Proxy @(VerKeyKES kes)))
      , ("vk1", info codec (Proxy @(VerKeyKES kes)))
      ]

--

-- *** 'CompactSingleKES'

instance
  ( DSIGNAlgorithm dsign
  , KESAlgorithm (CompactSingleKES dsign)
  ) =>
  HasInfo (DirectCodec m) (VerKeyKES (CompactSingleKES dsign))
  where
  info codec _ =
    aliasField
      ("VerKeyKES<" ++ algorithmNameKES (Proxy @(CompactSingleKES dsign)) ++ ">")
      (info codec (Proxy @(VerKeyDSIGN dsign)))

instance
  ( DSIGNAlgorithm dsign
  , KESAlgorithm (CompactSingleKES dsign)
  , HasInfo (DirectCodec m) (SignKeyDSIGN dsign)
  ) =>
  HasInfo (DirectCodec m) (SignKeyKES (CompactSingleKES dsign))
  where
  info codec _ =
    aliasField
      ("SignKeyKES<" ++ algorithmNameKES (Proxy @(CompactSingleKES dsign)) ++ ">")
      (info codec (Proxy @(SignKeyDSIGN dsign)))

-- *** 'CompactSumKES'

instance
  ( HashAlgorithm h
  , SodiumHashAlgorithm h
  , KESAlgorithm kes
  , KESAlgorithm (CompactSumKES h kes)
  ) =>
  HasInfo (DirectCodec m) (VerKeyKES (CompactSumKES h kes))
  where
  info codec _ =
    aliasField
      ("VerKeyKES<" ++ algorithmNameKES (Proxy @(CompactSumKES h kes)) ++ ">")
      (info codec (Proxy @(Hash h (VerKeyKES kes, VerKeyKES kes))))

instance
  ( SodiumHashAlgorithm h
  , KESAlgorithm kes
  , OptimizedKESAlgorithm kes
  , HasInfo (DirectCodec m) (SignKeyKES kes)
  , HasInfo (DirectCodec m) (VerKeyKES kes)
  , SizeHash h ~ SeedSizeKES kes
  , KnownNat (SizeSignKeyKES kes)
  , KnownNat (SizeVerKeyKES kes)
  , KnownNat (SeedSizeKES kes)
  , KnownNat ((SizeSignKeyKES kes + SeedSizeKES kes) + (SizeVerKeyKES kes * 2))
  , KnownNat (SizeSigKES kes + SizeVerKeyKES kes)
  , forall a. HasInfo (DirectCodec m) (Hash h a)
  ) =>
  HasInfo (DirectCodec m) (SignKeyKES (CompactSumKES h kes))
  where
  info codec _ =
    compoundField
      ("SignKeyKES<" ++ algorithmNameKES (Proxy @(CompactSumKES h kes)) ++ ">")
      [ ("sk", info codec (Proxy @(SignKeyKES kes)))
      , ("seed", basicField "bytes" (FixedSize (fromIntegral $ seedSizeKES (Proxy @kes))))
      , ("vk0", info codec (Proxy @(VerKeyKES kes)))
      , ("vk1", info codec (Proxy @(VerKeyKES kes)))
      ]

-- ** 'Timestamp'

instance HasInfo (DirectCodec m) Timestamp where
  info codec _ = info codec (Proxy @Word64)

instance (MonadThrow m, MonadST m) => Serializable (DirectCodec m) Timestamp where
  encode s (Timestamp p) =
    encode s (fromIntegral p :: Word64)
  decode s =
    Timestamp . (fromIntegral @Word64) <$> decode s

-- ** 'KESPeriod'

instance HasInfo (DirectCodec m) KESPeriod where
  info codec _ = info codec (Proxy @Word64)

instance (MonadThrow m, MonadST m) => Serializable (DirectCodec m) KESPeriod where
  encode s (KESPeriod p) =
    encode s (fromIntegral p :: Word64)
  decode s =
    KESPeriod . (fromIntegral @Word64) <$> decode s

-- ** 'SignKeyWithPeriodKES'

instance
  (KESAlgorithm kes, HasInfo (DirectCodec m) (SignKeyKES kes)) =>
  HasInfo (DirectCodec m) (SignKeyWithPeriodKES kes)
  where
  info codec _ =
    compoundField
      ("SignKeyWithPeriodKES " ++ algorithmNameKES (Proxy @kes))
      [ ("signKey", info codec (Proxy @(SignKeyKES kes)))
      , ("period", info codec (Proxy @Period))
      ]

instance
  ( MonadThrow m
  , MonadST m
  , KESAlgorithm kes
  , Serializable (DirectCodec m) (SignKeyKES kes)
  ) =>
  Serializable (DirectCodec m) (SignKeyWithPeriodKES kes)
  where
  encode s skp = do
    encode s (skWithoutPeriodKES skp)
    encode s (periodKES skp)

  decode s =
    SignKeyWithPeriodKES <$> decode s <*> decode s

-- ** 'Period'

instance HasInfo (DirectCodec m) Period where
  info codec _ = aliasField "Period" $ info codec (Proxy @Word32)

instance (MonadThrow m, MonadST m) => Serializable (DirectCodec m) Period where
  encode codec = encodeWith $ \s -> sendWord32 s . fromIntegral
  decode codec = fromIntegral @Word32 <$> decode codec

-- ** 'OCert'

-- $(deriveSerDoc ''DirectCodec [] ''OCert)
instance
  ( HasInfo (DirectCodec m_axB5) (VerKeyKES (KES c_ieDN))
  , HasInfo (DirectCodec m_axB5) (SignedDSIGN (DSIGN c_ieDN) (OCertSignable c_ieDN))
  ) =>
  HasInfo (DirectCodec m_axB5) (OCert c_ieDN)
  where
  info codec _ =
    (compoundField "OCert")
      [
        ( "ocertVkHot"
        , (info codec) (Proxy :: Proxy (VerKeyKES (KES c_ieDN)))
        )
      , ("ocertN", (info codec) (Proxy :: Proxy Word64))
      , ("ocertKESPeriod", (info codec) (Proxy :: Proxy KESPeriod))
      ,
        ( "ocertSigma"
        , (info codec)
            ( Proxy ::
                Proxy (SignedDSIGN (DSIGN c_ieDN) (OCertSignable c_ieDN))
            )
        )
      ]
instance
  ( Monad (MonadEncode (DirectCodec m_axB5))
  , Monad (MonadDecode (DirectCodec m_axB5))
  , Serializable (DirectCodec m_axB5) (VerKeyKES (KES c_ieDN))
  , Serializable (DirectCodec m_axB5) Word64
  , Serializable (DirectCodec m_axB5) KESPeriod
  , Serializable (DirectCodec m_axB5) (SignedDSIGN (DSIGN c_ieDN) (OCertSignable c_ieDN))
  ) =>
  Serializable (DirectCodec m_axB5) (OCert c_ieDN)
  where
  encode p item =
    sequence_
      [ (encode p) (ocertVkHot item)
      , (encode p) (ocertN item)
      , (encode p) (ocertKESPeriod item)
      , (encode p) (ocertSigma item)
      ]
  decode p =
    ((((OCert <$> decode p) <*> decode p) <*> decode p) <*> decode p)

-- ** 'TaggedBundle'

instance
  ( HasInfo (DirectCodec m) (Bundle m c)
  , Typeable c
  , Crypto c
  , KESAlgorithm (KES c)
  ) =>
  HasInfo (DirectCodec m) (TaggedBundle m c)
  where
  info codec _ =
    compoundField
      ("TaggedBundle " ++ algorithmNameKES (Proxy @(KES c)) ++ " " ++ algorithmNameDSIGN (Proxy @(DSIGN c)))
      [ ("taggedBundle", info codec (Proxy @(Bundle m c)))
      , ("taggedTimestamp", info codec (Proxy @Timestamp))
      ]

instance
  ( MonadThrow m
  , MonadST m
  , MonadSTM m
  , MonadMVar m
  , Serializable (DirectCodec m) (Bundle m c)
  , Typeable c
  , Crypto c
  , KESAlgorithm (KES c)
  ) =>
  Serializable (DirectCodec m) (TaggedBundle m c)
  where
  encode codec = encodeWith $ \s tbundle -> do
    runReaderT
      ( do
          encode codec $ taggedBundle tbundle
          encode codec $ taggedBundleTimestamp tbundle
      )
      s

  decode codec = do
    b <- decode codec
    t <- decode codec
    return
      TaggedBundle
        { taggedBundle = b
        , taggedBundleTimestamp = t
        }

-- ** 'Bundle'

instance
  ( HasInfo (DirectCodec m) (SignKeyKES (KES c))
  , HasInfo (DirectCodec m) (OCert c)
  , Typeable c
  , Crypto c
  , KESAlgorithm (KES c)
  ) =>
  HasInfo (DirectCodec m) (Bundle m c)
  where
  info codec _ =
    compoundField
      ("Bundle " ++ algorithmNameKES (Proxy @(KES c)) ++ " " ++ algorithmNameDSIGN (Proxy @(DSIGN c)))
      [ ("signKeyWithPeriod", info codec (Proxy @(SignKeyWithPeriodKES (KES c))))
      , ("ocert", info codec (Proxy @(OCert c)))
      ]

instance
  ( MonadThrow m
  , MonadST m
  , MonadSTM m
  , MonadMVar m
  , Serializable (DirectCodec m) (SignKeyWithPeriodKES (KES c))
  , Serializable (DirectCodec m) (OCert c)
  , Typeable c
  , Crypto c
  , KESAlgorithm (KES c)
  ) =>
  Serializable (DirectCodec m) (Bundle m c)
  where
  encode codec = encodeWith $ \s bundle -> do
    withCRefValue (bundleSKP bundle) $ \skp -> do
      runReaderT (encode codec skp) s
    runReaderT (encode codec (bundleOC bundle)) s

  decode codec = do
    skp <- decode codec
    skpVar <-
      lift . lift $
        newCRef
          (forgetSignKeyKES . skWithoutPeriodKES)
          skp
    oc <- decode codec
    return
      Bundle
        { bundleSKP = skpVar
        , bundleOC = oc
        }

instance DSIGNAlgorithm dsign => HasInfo (DirectCodec m) (SigDSIGN dsign) where
  info _ _ =
    aliasField
      ("SigDSIGN " ++ algorithmNameDSIGN (Proxy @dsign))
      $ basicField
        "bytes"
        (FixedSize . fromIntegral $ sizeSigDSIGN (Proxy @dsign))
instance
  ( MonadST m
  , MonadSTM m
  , MonadThrow m
  , DSIGNAlgorithm dsign
  ) =>
  Serializable (DirectCodec m) (SigDSIGN dsign)
  where
  encode codec val =
    encodeSized (fromIntegral $ sizeSigDSIGN (Proxy @dsign)) (rawSerialiseSigDSIGN val)

  decode codec = do
    raw <- decodeSized (fromIntegral $ sizeSigDSIGN (Proxy @dsign))
    let deser = rawDeserialiseSigDSIGN raw
    case deser of
      Nothing -> lift . ReadResultT . return $ ReadMalformed "Invalid serialised SigDSIGN"
      Just sig -> return sig

-- ** 'RecvResult'

deriving via
  (ViaEnum RecvResult)
  instance
    HasInfo (DirectCodec m) RecvResult

instance
  ( MonadThrow m
  , MonadST m
  ) =>
  Serializable (DirectCodec m) RecvResult
  where
  encode codec = encodeEnum codec (Proxy @(DefEnumEncoding (DirectCodec m)))
  decode codec = decodeEnum codec (Proxy @(DefEnumEncoding (DirectCodec m)))

-- ** 'VersionIdentifier'

instance HasInfo (DirectCodec m) VersionIdentifier where
  info _ _ =
    aliasField
      "VersionIdentifier"
      $ basicField
        "bytes"
        (FixedSize versionIdentifierLength)

instance
  ( MonadThrow m
  , MonadST m
  ) =>
  Serializable (DirectCodec m) VersionIdentifier
  where
  encode _ = encodeSized versionIdentifierLength . unVersionIdentifier
  decode _ = VersionIdentifier <$> decodeSized versionIdentifierLength
