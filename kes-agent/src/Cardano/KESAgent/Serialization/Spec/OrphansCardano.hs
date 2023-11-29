{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.KESAgent.Serialization.Spec.OrphansCardano
where

import Cardano.KESAgent.KES.Bundle
import Cardano.KESAgent.KES.Crypto
import Cardano.KESAgent.KES.OCert
import Cardano.KESAgent.Protocols.RecvResult
import Cardano.KESAgent.Protocols.VersionedProtocol
import Cardano.KESAgent.Serialization.RawUtil
import Cardano.KESAgent.Serialization.Spec.Types
import Cardano.KESAgent.Serialization.Spec.Class
import Cardano.KESAgent.Serialization.Spec.OrphansBase
import Cardano.KESAgent.Serialization.Spec.TH
import Cardano.KESAgent.Util.RefCounting

import Cardano.Binary
import Cardano.Crypto.DirectSerialise
import Cardano.Crypto.DSIGN.Class
import Cardano.Crypto.KES.Class
import Cardano.Crypto.KES.Single
import Cardano.Crypto.KES.Sum
import Cardano.Crypto.KES.CompactSingle
import Cardano.Crypto.KES.CompactSum
import Cardano.Crypto.KES.Mock
import Cardano.Crypto.Libsodium.Hash.Class
import Cardano.Crypto.Hash.Class
import Ouroboros.Network.RawBearer ( RawBearer (..) )

import Control.Monad ( when )
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadSTM
import Control.Concurrent.Class.MonadMVar
import Control.Monad.Class.MonadThrow ( MonadThrow, throwIO )
import Data.Coerce
import Data.Proxy
import Data.Typeable
import Data.Word
import Foreign.Ptr ( castPtr )
import GHC.TypeLits ( KnownNat, type (+), type (*) )
import Control.Tracer ( nullTracer )
import Control.Monad.Trans
import Network.TypedProtocol.Core

-- ** 'SodiumHashAlgorithm'

instance ( SodiumHashAlgorithm h
         , HashAlgorithm h
         ) => HasSerInfo (Hash h a) where
    info _ =
      aliasField ("Hash<" ++ hashAlgorithmName (Proxy @h) ++ ">")
        $ basicField
            "bytes"
            (FixedSize . fromIntegral $ sizeHash (Proxy @h))

-- ** DSIGN

instance ( DSIGNAlgorithm dsign ) => HasSerInfo (VerKeyDSIGN dsign) where
    info _ =
      aliasField ("VerKeyDSIGN " ++ algorithmNameDSIGN (Proxy @dsign))
        $ basicField
            "bytes"
            (FixedSize . fromIntegral $ sizeVerKeyDSIGN (Proxy @dsign))

instance ( MonadST m
         , MonadSTM m
         , MonadThrow m
         , DSIGNAlgorithm dsign
         ) => IsSerItem m (VerKeyDSIGN dsign) where
    sendItem s val =
      sendItem s (Sized (rawSerialiseVerKeyDSIGN val) :: Sized (SizeVerKeyDSIGN dsign) ByteString)

    receiveItem s = do
      sized <- receiveItem s
      let deser = rawDeserialiseVerKeyDSIGN $ unSized (sized :: Sized (SizeVerKeyDSIGN dsign) ByteString)
      case deser of
        Nothing -> ReadResultT . return $ ReadMalformed "Invalid serialised VerKeyDSIGN"
        Just vk -> return vk

instance ( DSIGNAlgorithm dsign
         ) => HasSerInfo (SignKeyDSIGN dsign) where
   info _ =
     aliasField
       ("SignKeyDSIGN<" ++ algorithmNameDSIGN (Proxy @dsign) ++ ">")
       $ basicField
          "bytes"
          (FixedSize . fromIntegral $ sizeSignKeyDSIGN (Proxy @dsign))

instance ( MonadST m
         , MonadSTM m
         , MonadThrow m
         , DSIGNAlgorithm dsign
         ) => IsSerItem m (SignKeyDSIGN dsign) where
    sendItem s val =
      sendItem s (Sized (rawSerialiseSignKeyDSIGN val) :: Sized (SizeSignKeyDSIGN dsign) ByteString)

    receiveItem s = do
      sized <- receiveItem s
      let deser = rawDeserialiseSignKeyDSIGN $ unSized (sized :: Sized (SizeSignKeyDSIGN dsign) ByteString)
      case deser of
        Nothing -> ReadResultT . return $ ReadMalformed "Invalid serialised SignKeyDSIGN"
        Just vk -> return vk

instance HasSerInfo (SigDSIGN d) => HasSerInfo (SignedDSIGN d a) where
  info _ =
    info (Proxy @(SigDSIGN d))

instance (Monad m, IsSerItem m (SigDSIGN d)) => IsSerItem m (SignedDSIGN d a) where
  sendItem s (SignedDSIGN sig) = sendItem s sig
  receiveItem s = SignedDSIGN <$> receiveItem s


-- ** KES
-- We use the same actual ser/deser code for all KES algorithms, but the
-- 'HasSerInfo' implementations are different, in order to reflect the actual
-- data structures.

instance ( MonadSTM m
         , MonadThrow m
         , MonadST m
         , DirectSerialise (SignKeyKES kes)
         , DirectDeserialise (SignKeyKES kes)
         , KESAlgorithm kes
         , HasSerInfo (SignKeyKES kes)
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

instance ( MonadST m
         , MonadSTM m
         , MonadST m
         , MonadThrow m
         , KESAlgorithm kes
         , HasSerInfo (VerKeyKES kes)
         ) => IsSerItem m (VerKeyKES kes) where
    sendItem s val =
      sendItem s (Sized (rawSerialiseVerKeyKES val) :: Sized (SizeVerKeyKES kes) ByteString)

    receiveItem s = do
      sized <- receiveItem s
      let deser = rawDeserialiseVerKeyKES $ unSized (sized :: Sized (SizeVerKeyKES kes) ByteString)
      case deser of
        Nothing -> ReadResultT . return $ ReadMalformed "Invalid serialised VerKeyKES"
        Just vk -> return vk

-- *** 'MockKES'

instance ( KnownNat t
         , KESAlgorithm (MockKES t)
         , HasSerInfo (SignKeyKES (MockKES t))
         ) => HasSerInfo (VerKeyKES (MockKES t)) where
    info _ =
      aliasField
        ("VerKeyKES<" ++ algorithmNameKES (Proxy @(MockKES t)) ++ ">")
        (info (Proxy @Word64))


instance ( KnownNat t
         , KESAlgorithm (MockKES t)
         ) => HasSerInfo (SignKeyKES (MockKES t)) where
   info _ =
     compoundField
        ("SignKeyKES<" ++ algorithmNameKES (Proxy @(MockKES t)) ++ ">")
        [ ("verKey", info (Proxy @(VerKeyKES (MockKES t))))
        , ("period", info (Proxy @Word64))
        ]

-- *** 'SingleKES'

instance ( DSIGNAlgorithm dsign
         , KESAlgorithm (SingleKES dsign)
         ) => HasSerInfo (VerKeyKES (SingleKES dsign)) where
    info _ =
      aliasField
        ("VerKeyKES<" ++ algorithmNameKES (Proxy @(SingleKES dsign)) ++ ">")
        (info (Proxy @(VerKeyDSIGN dsign)))


instance ( DSIGNAlgorithm dsign
         , KESAlgorithm (SingleKES dsign)
         , HasSerInfo (SignKeyDSIGN dsign)
         ) => HasSerInfo (SignKeyKES (SingleKES dsign)) where
   info _ =
     aliasField
        ("SignKeyKES<" ++ algorithmNameKES (Proxy @(SingleKES dsign)) ++ ">")
        (info (Proxy @(SignKeyDSIGN dsign)))

-- *** 'SumKES'

instance ( HashAlgorithm h
         , SodiumHashAlgorithm h
         , KESAlgorithm kes
         , KESAlgorithm (SumKES h kes)
         ) => HasSerInfo (VerKeyKES (SumKES h kes)) where
    info _ =
      aliasField
        ("VerKeyKES<" ++ algorithmNameKES (Proxy @(SumKES h kes)) ++ ">")
        (info (Proxy @(Hash h (VerKeyKES kes, VerKeyKES kes))))


instance ( SodiumHashAlgorithm h
         , KESAlgorithm kes
         , HasSerInfo (SignKeyKES kes)
         , HasSerInfo (VerKeyKES kes)
         , SizeHash h ~ SeedSizeKES kes
         , KnownNat (SizeSignKeyKES kes)
         , KnownNat (SizeVerKeyKES kes)
         , KnownNat (SeedSizeKES kes)
         , KnownNat ((SizeSignKeyKES kes + SeedSizeKES kes) + (2 * SizeVerKeyKES kes))
         , KnownNat (SizeSigKES kes + (SizeVerKeyKES kes * 2))
         , forall a. HasSerInfo (Hash h a)
         ) => HasSerInfo (SignKeyKES (SumKES h kes)) where
   info _ =
     compoundField
        ("SignKeyKES<" ++ algorithmNameKES (Proxy @(SumKES h kes)) ++ ">")
        [ ("sk", info (Proxy @(SignKeyKES kes)))
        , ("seed", basicField "bytes" (FixedSize (fromIntegral $ seedSizeKES (Proxy @kes))))
        , ("vk0", info (Proxy @(VerKeyKES kes)))
        , ("vk1", info (Proxy @(VerKeyKES kes)))
        ]
--
-- *** 'CompactSingleKES'

instance ( DSIGNAlgorithm dsign
         , KESAlgorithm (CompactSingleKES dsign)
         ) => HasSerInfo (VerKeyKES (CompactSingleKES dsign)) where
    info _ =
      aliasField
        ("VerKeyKES<" ++ algorithmNameKES (Proxy @(CompactSingleKES dsign)) ++ ">")
        (info (Proxy @(VerKeyDSIGN dsign)))


instance ( DSIGNAlgorithm dsign
         , KESAlgorithm (CompactSingleKES dsign)
         , HasSerInfo (SignKeyDSIGN dsign)
         ) => HasSerInfo (SignKeyKES (CompactSingleKES dsign)) where
   info _ =
     aliasField
        ("SignKeyKES<" ++ algorithmNameKES (Proxy @(CompactSingleKES dsign)) ++ ">")
        (info (Proxy @(SignKeyDSIGN dsign)))

-- *** 'CompactSumKES'

instance ( HashAlgorithm h
         , SodiumHashAlgorithm h
         , KESAlgorithm kes
         , KESAlgorithm (CompactSumKES h kes)
         ) => HasSerInfo (VerKeyKES (CompactSumKES h kes)) where
    info _ =
      aliasField
        ("VerKeyKES<" ++ algorithmNameKES (Proxy @(CompactSumKES h kes)) ++ ">")
        (info (Proxy @(Hash h (VerKeyKES kes, VerKeyKES kes))))


instance ( SodiumHashAlgorithm h
         , KESAlgorithm kes
         , OptimizedKESAlgorithm kes
         , HasSerInfo (SignKeyKES kes)
         , HasSerInfo (VerKeyKES kes)
         , SizeHash h ~ SeedSizeKES kes
         , KnownNat (SizeSignKeyKES kes)
         , KnownNat (SizeVerKeyKES kes)
         , KnownNat (SeedSizeKES kes)
         , KnownNat ((SizeSignKeyKES kes + SeedSizeKES kes) + (SizeVerKeyKES kes * 2))
         , KnownNat (SizeSigKES kes + SizeVerKeyKES kes)
         , forall a. HasSerInfo (Hash h a)
         ) => HasSerInfo (SignKeyKES (CompactSumKES h kes)) where
   info _ =
     compoundField
        ("SignKeyKES<" ++ algorithmNameKES (Proxy @(CompactSumKES h kes)) ++ ">")
        [ ("sk", info (Proxy @(SignKeyKES kes)))
        , ("seed", basicField "bytes" (FixedSize (fromIntegral $ seedSizeKES (Proxy @kes))))
        , ("vk0", info (Proxy @(VerKeyKES kes)))
        , ("vk1", info (Proxy @(VerKeyKES kes)))
        ]


-- ** 'KESPeriod'

instance HasSerInfo KESPeriod where
  info _ = info (Proxy @Word64)

instance (MonadThrow m, MonadST m) => IsSerItem m KESPeriod where
  sendItem s (KESPeriod p) =
    sendItem s (fromIntegral p :: Word64)
  receiveItem s =
    KESPeriod . (fromIntegral @Word64) <$> receiveItem s

-- ** 'SignKeyWithPeriodKES'

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

-- ** 'Period'

instance HasSerInfo Period where
  info _ = aliasField "Period" $ info (Proxy @Word32)

instance (MonadThrow m, MonadST m) => IsSerItem m Period where
  sendItem s = sendWord32 s . fromIntegral
  receiveItem s = fromIntegral <$> (receiveItem s :: ReadResultT m Word32)


-- ** 'OCert'

$(deriveSerWithCrypto ''OCert)

-- ** 'Bundle'

instance ( HasSerInfo (SignKeyKES (KES c))
         , HasSerInfo (VerKeyKES (KES c))
         , Typeable c
         , Crypto c
         , KESAlgorithm (KES c)
         )
         => HasSerInfo (Bundle m c) where
  info _ =
    compoundField
      ("Bundle " ++ algorithmNameKES (Proxy @(KES c)) ++ " " ++ algorithmNameDSIGN (Proxy @(DSIGN c)))
      [ ("signKeyWithPeriod", info (Proxy @(SignKeyWithPeriodKES (KES c))))
      , ("ocert", info (Proxy @(OCert c)))
      ]

instance ( forall a b. Coercible a b => Coercible (m a) (m b)
         , MonadThrow m
         , MonadST m
         , MonadSTM m
         , MonadMVar m
         , IsSerItem m (SignKeyKES (KES c))
         , IsSerItem m (VerKeyKES (KES c))
         , DirectSerialise (SignKeyKES (KES c))
         , DirectDeserialise (SignKeyKES (KES c))
         , Typeable c
         , Crypto c
         , KESAlgorithm (KES c)
         )
         => IsSerItem m (Bundle m c) where
    sendItem s bundle = do
      withCRefValue (bundleSKP bundle) $ \skp -> do
        sendItem s skp
      sendItem s (bundleOC bundle)

    receiveItem s = do
      skp <- receiveItem s
      skpVar <- lift $ newCRef
        (forgetSignKeyKES . skWithoutPeriodKES)
        skp
      oc <- receiveItem s
      return Bundle
        { bundleSKP = skpVar
        , bundleOC = oc
        }


instance ( DSIGNAlgorithm dsign ) => HasSerInfo (SigDSIGN dsign) where
    info _ =
      aliasField
        ("SigDSIGN " ++ algorithmNameDSIGN (Proxy @dsign))
        $ basicField
            "bytes"
            (FixedSize . fromIntegral $ sizeSigDSIGN (Proxy @dsign))
instance ( MonadST m
         , MonadSTM m
         , MonadThrow m
         , DSIGNAlgorithm dsign
         ) => IsSerItem m (SigDSIGN dsign) where
    sendItem s val =
      sendItem s (Sized (rawSerialiseSigDSIGN val) :: Sized (SizeSigDSIGN dsign) ByteString)

    receiveItem s = do
      sized <- receiveItem s
      let deser = rawDeserialiseSigDSIGN $ unSized (sized :: Sized (SizeSigDSIGN dsign) ByteString)
      case deser of
        Nothing -> ReadResultT . return $ ReadMalformed "Invalid serialised SigDSIGN"
        Just vk -> return vk


-- ** 'RecvResult'

deriving via (ViaEnum RecvResult)
  instance HasSerInfo RecvResult

deriving via (ViaEnum RecvResult)
  instance
    ( forall x y. Coercible x y => Coercible (m x) (m y)
    , MonadThrow m
    , MonadST m
    ) => IsSerItem m RecvResult

-- ** 'VersionIdentifier'

deriving via (Sized VersionIdentifierLength ByteString)
  instance HasSerInfo VersionIdentifier
deriving via (Sized VersionIdentifierLength ByteString)
  instance
    ( forall a b. Coercible a b => Coercible (m a) (m b)
    , MonadThrow m
    , MonadST m
    ) => IsSerItem m VersionIdentifier
