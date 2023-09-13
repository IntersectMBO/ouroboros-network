{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.KESAgent.Serialization.Spec
where

import Cardano.KESAgent.Serialization.RawUtil
import Cardano.KESAgent.KES.Crypto
import Cardano.KESAgent.KES.OCert
import Cardano.KESAgent.KES.Bundle
import Cardano.KESAgent.Util.RefCounting

import Cardano.Crypto.DirectSerialise
import Cardano.Crypto.KES.Class
import Cardano.Crypto.DSIGN.Class
import Ouroboros.Network.RawBearer ( RawBearer (..) )
import Cardano.Binary

import Data.Word
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Kind
import Data.Proxy
import Data.Typeable
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Control.Monad ( void, unless, when )
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow ( MonadThrow, throwIO, catch )
import GHC.TypeLits
import Foreign.Ptr ( castPtr )
import Control.Monad.Trans ( lift )

data FieldInfo
  = BasicField BasicFieldInfo
  | CompoundField CompoundFieldInfo

data FieldSize
  = FixedSize !Int
  | VarSize !String
  | SumSize !FieldSize !FieldSize
  | MulSize !FieldSize !FieldSize

data BasicFieldInfo =
  BasicFieldInfo
    { fieldType :: !String
    , fieldSize :: !FieldSize
    }

data CompoundFieldInfo =
  CompoundFieldInfo
    { compoundFieldType :: !String
    , compoundFieldSubfields :: ![CompoundSubfieldInfo]
    }

data CompoundSubfieldInfo =
  CompoundSubfieldInfo
    { subfieldName :: !String
    , subfieldInfo :: !FieldInfo
    }

basicField :: String -> FieldSize -> FieldInfo
basicField ty size = BasicField $ BasicFieldInfo ty size

compoundField :: String -> [(String, FieldInfo)] -> FieldInfo
compoundField ty subfields =
  CompoundField $
    CompoundFieldInfo
      ty
      [ CompoundSubfieldInfo name i
      | (name, i) <- subfields
      ]
  
class HasSerInfo a where
  info :: forall (proxy :: Type -> Type). proxy a -> FieldInfo

class HasSerInfo a => IsSerItem m a where
  sendItem :: RawBearer m -> a -> m ()
  receiveItem :: RawBearer m -> ReadResultT m a

instance HasSerInfo Word8 where
  info _ = basicField "Word8" $ FixedSize 8
  
instance (MonadThrow m, MonadST m) => IsSerItem m Word8 where
  sendItem = sendWord8
  receiveItem s = ReadResultT $ receiveWord8 s

instance HasSerInfo Word16 where
  info _ = basicField "Word16" $ FixedSize 16
  
instance (MonadThrow m, MonadST m) => IsSerItem m Word16 where
  sendItem = sendWord16
  receiveItem s = ReadResultT $ receiveWord16 s

instance HasSerInfo Word32 where
  info _ = basicField "Word32" $ FixedSize 32
  
instance (MonadThrow m, MonadST m) => IsSerItem m Word32 where
  sendItem = sendWord32
  receiveItem s = ReadResultT $ receiveWord32 s

instance HasSerInfo Word64 where
  info _ = basicField "Word64" $ FixedSize 64
  
instance (MonadThrow m, MonadST m) => IsSerItem m Word64 where
  sendItem = sendWord64
  receiveItem s = ReadResultT $ receiveWord64 s

newtype Sized (len :: Nat) a = Sized { unSized :: a }

instance (KnownNat len) => HasSerInfo (Sized len ByteString) where
  info _ =
    let n = fromIntegral $ natVal (Proxy @len)
    in basicField
        ("ByteString[" ++ show n ++ "]")
        $ FixedSize n
instance (MonadThrow m, MonadST m, KnownNat len) => IsSerItem m (Sized len ByteString) where
  sendItem s val = do
    actualLen <- sendBS s (unSized val)
    let expectedLen = fromIntegral $ natVal (Proxy @len)
    unless (actualLen == expectedLen) (error "Length mismatch")

  receiveItem s = do
    let n = fromIntegral $ natVal (Proxy @len)
    Sized <$> ReadResultT (receiveBS s (fromIntegral n))


newtype VariableSized a = VariableSized { unVariableSized :: a }

instance HasSerInfo (VariableSized ByteString) where
  info _ =
    compoundField "ByteString"
      [ ("length", info (Proxy @Word32))
      , ("data", basicField "[Word8]" $ VarSize "length")
      ]
instance (MonadThrow m, MonadST m) => IsSerItem m (VariableSized ByteString) where
  sendItem s val = do
    let len = BS.length (unVariableSized val)
    sendWord32 s (fromIntegral len)
    actualLen <- sendBS s (unVariableSized val)
    unless (actualLen == len) (error "Length mismatch")

  receiveItem s = do
    len <- ReadResultT (receiveWord32 s)
    VariableSized <$> ReadResultT (receiveBS s (fromIntegral len))


instance ( KESAlgorithm kes
         ) => HasSerInfo (SignKeyKES kes) where
    info _ =
      basicField
        ("SignKeyKES " ++ algorithmNameKES (Proxy @kes))
        (FixedSize . fromIntegral $ sizeSignKeyKES (Proxy @kes))
instance ( MonadSTM m
         , MonadThrow m
         , DirectSerialise m (SignKeyKES kes)
         , DirectDeserialise m (SignKeyKES kes)
         , KESAlgorithm kes
         ) => IsSerItem m (SignKeyKES kes) where
    sendItem s val = do
      directSerialise
        (\buf bufSize -> do
          n <- send s (castPtr buf) (fromIntegral bufSize)
          when (fromIntegral n /= bufSize) (error "AAAAA")
        ) val

    receiveItem s = ReadResultT $ do
      sk <- directDeserialise
        (\buf bufSize -> do
            unsafeReceiveN s buf bufSize >>= \case
              ReadOK n -> do
                when (fromIntegral n /= bufSize) (throwIO (ReadMalformed "Incorrect number of key bytes" :: ReadResult ()))
              x ->  do
                throwIO x
        )
      return $ ReadOK sk


instance ( KESAlgorithm kes ) => HasSerInfo (VerKeyKES kes) where
    info _ =
      basicField
        ("VerKeyKES " ++ algorithmNameKES (Proxy @kes))
        (FixedSize . fromIntegral $ sizeVerKeyKES (Proxy @kes))
instance ( MonadSTM m
         , MonadThrow m
         , DirectSerialise m (VerKeyKES kes)
         , DirectDeserialise m (VerKeyKES kes)
         , KESAlgorithm kes
         ) => IsSerItem m (VerKeyKES kes) where
    sendItem s val = do
      directSerialise
        (\buf bufSize -> do
          n <- send s (castPtr buf) (fromIntegral bufSize)
          when (fromIntegral n /= bufSize) (error "AAAAA")
        ) val

    receiveItem s = ReadResultT $ do
      vk <- directDeserialise
        (\buf bufSize -> do
            unsafeReceiveN s buf bufSize >>= \case
              ReadOK n -> do
                when (fromIntegral n /= bufSize) (throwIO (ReadMalformed "Incorrect number of key bytes" :: ReadResult ()))
              x ->  do
                throwIO x
        )
      return $ ReadOK vk


instance ( Crypto c , Typeable c) => HasSerInfo (OCert c) where
    info _ =
      compoundField
        ("OCert " ++ algorithmNameKES (Proxy @(KES c)) ++ " " ++ algorithmNameDSIGN (Proxy @(DSIGN c)))
        [ ("size", info (Proxy @Word32))
        , ("data", basicField "raw-serialized OCert" (VarSize "size"))
        ]
instance ( MonadThrow m, MonadST m, Crypto c , Typeable c) => IsSerItem m (OCert c) where
    sendItem s oc =
      sendItem s (VariableSized . serialize' $ oc)

    receiveItem s =
      unsafeDeserialize' . unVariableSized <$> receiveItem s

instance HasSerInfo a => HasSerInfo (CRef m a) where
  info (p :: proxy (CRef m a)) = info (Proxy @a)

class ToCRef m a where
  toCRef :: a -> m (CRef m a)

instance (MonadThrow m, MonadSTM m, ToCRef m a, IsSerItem m a) => IsSerItem m (CRef m a) where
  sendItem s var = withCRefValue var $ sendItem s
  receiveItem s = receiveItem s >>= lift . toCRef

instance (MonadThrow m, MonadSTM m, MonadST m, KESAlgorithm kes)
          => ToCRef m (SignKeyWithPeriodKES kes) where
  toCRef = newCRef (forgetSignKeyKES . skWithoutPeriodKES)

instance HasSerInfo Period where
  info _ = basicField "Period" (FixedSize 32)

instance (MonadThrow m, MonadST m) => IsSerItem m Period where
  sendItem s = sendWord32 s . fromIntegral
  receiveItem s = fromIntegral <$> (receiveItem s :: ReadResultT m Word32)


instance (KESAlgorithm kes, HasSerInfo (SignKeyKES kes))
          => HasSerInfo (SignKeyWithPeriodKES kes) where
  info _ =
    compoundField
      ("SignKeyWithPeriodKES " ++ algorithmNameKES (Proxy @kes))
      [ ("signKey", info (Proxy @(SignKeyKES kes)))
      , ("period", info (Proxy @Period))
      ]

instance ( MonadThrow m
         , MonadST m
         , KESAlgorithm kes
         , IsSerItem m (SignKeyKES kes)
         )
         => IsSerItem m (SignKeyWithPeriodKES kes) where
  sendItem s skp = do
    sendItem s (skWithoutPeriodKES skp)
    sendItem s (periodKES skp)

  receiveItem s =
    SignKeyWithPeriodKES <$> receiveItem s <*> receiveItem s

instance ( HasSerInfo (SignKeyKES (KES c))
         , Typeable c
         , Crypto c
         , KESAlgorithm (KES c)
         )
         => HasSerInfo (Bundle m c) where
  info _ =
    compoundField
      ("Bundle " ++ algorithmNameKES (Proxy @(KES c)) ++ " " ++ algorithmNameDSIGN (Proxy @(DSIGN c)))
      [ ("signKeyWithPeriod", info (Proxy @(CRef m (SignKeyWithPeriodKES (KES c)))))
      , ("ocert", info (Proxy @(OCert c)))
      ]

instance ( MonadThrow m
         , MonadST m
         , MonadSTM m
         , IsSerItem m (SignKeyKES (KES c))
         , Typeable c
         , Crypto c
         , KESAlgorithm (KES c)
         )
         => IsSerItem m (Bundle m c) where
    sendItem s bundle = do
      sendItem s (bundleSKP bundle)
      sendItem s (bundleOC bundle)

    receiveItem s =
      Bundle <$> receiveItem s <*> receiveItem s

newtype ViaEnum a = ViaEnum { viaEnum :: a }

instance (Typeable a) => HasSerInfo (ViaEnum a) where
  info _ =
    basicField
      "Enum"
      (FixedSize 32)

typeName :: Typeable a => Proxy a -> String
typeName = tyConName . typeRepTyCon . typeRep

instance (MonadThrow m, MonadST m, Typeable a, Enum a, Bounded a) => IsSerItem m (ViaEnum a) where
  sendItem s (ViaEnum x) =
    sendItem s (fromIntegral (fromEnum x) :: Word32)

  receiveItem s = do
    nw :: Word32 <- receiveItem s
    let n = fromIntegral nw
    if n < fromEnum (minBound :: a) ||
       n > fromEnum (maxBound :: a) then
      ReadResultT $ return (ReadMalformed (typeName (Proxy @a)))
    else
      return (ViaEnum $ toEnum n)
