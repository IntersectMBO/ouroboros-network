{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}


module Cardano.Api.Any (
    module Cardano.Api.Any
  , module Cbor
  , module Proxy
  ) where


import           Control.Exception (Exception (..), IOException, throwIO)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16 (decode, encode)
import           Data.Kind (Constraint, Type)
import           Data.Proxy as Proxy (Proxy (..))
import           Data.Text as Text (Text)
import qualified Data.Text.Encoding as Text (decodeUtf8)
import           System.IO (Handle)

import           Cardano.Ledger.Binary as Cbor (FromCBOR (..), ToCBOR (..), EncCBOR(..), DecCBOR(..))
import qualified Cardano.Ledger.Binary.Plain as CBOR



-- DUPLICATE -- adapted from: cardano-api/src/Cardano/Api/HasTypeProxy.hs

class HasTypeProxy t where
  -- | A family of singleton types used in this API to indicate which type to
  -- use where it would otherwise be ambiguous or merely unclear.
  --
  -- Values of this type are passed to deserialisation functions for example.
  --
  data AsType t

  proxyToAsType :: Proxy t -> AsType t


data FromSomeType (c :: Type -> Constraint) b where
     FromSomeType :: c a => AsType a -> (a -> b) -> FromSomeType c b



-- DUPLICATE -- adapted from: cardano-api/src/Cardano/Api/Hash.hs

data family Hash keyrole :: Type

class CastHash roleA roleB where

    castHash :: Hash roleA -> Hash roleB


instance HasTypeProxy a => HasTypeProxy (Hash a) where
    data AsType (Hash a) = AsHash (AsType a)
    proxyToAsType _ = AsHash (proxyToAsType (Proxy :: Proxy a))



-- DUPLICATE -- adapted from: cardano-api/src/Cardano/Api/SerialiseRaw.hs

class HasTypeProxy a => SerialiseAsRawBytes a where

  serialiseToRawBytes :: a -> ByteString

  deserialiseFromRawBytes :: AsType a -> ByteString -> Maybe a

serialiseToRawBytesHex :: SerialiseAsRawBytes a => a -> ByteString
serialiseToRawBytesHex = Base16.encode . serialiseToRawBytes

serialiseToRawBytesHexText :: SerialiseAsRawBytes a => a -> Text
serialiseToRawBytesHexText = Text.decodeUtf8 . serialiseToRawBytesHex

deserialiseFromRawBytesHex :: SerialiseAsRawBytes a
                           => AsType a -> ByteString -> Maybe a
deserialiseFromRawBytesHex proxy hex =
    case Base16.decode hex of
      Right raw -> deserialiseFromRawBytes proxy raw
      Left _msg -> Nothing



-- DUPLICATE -- adapted from: cardano-api/src/Cardano/Api/SerialiseAsCBOR.hs

class HasTypeProxy a => SerialiseAsCBOR a where
    serialiseToCBOR :: a -> ByteString
    deserialiseFromCBOR :: AsType a -> ByteString -> Either CBOR.DecoderError a

    default serialiseToCBOR :: ToCBOR a => a -> ByteString
    serialiseToCBOR = CBOR.serialize'

    default deserialiseFromCBOR :: FromCBOR a
                                => AsType a
                                -> ByteString
                                -> Either CBOR.DecoderError a
    deserialiseFromCBOR _proxy = CBOR.decodeFull'



-- DUPLICATE -- adapted from: cardano-api/src/Cardano/Api/Error.hs

class Show e => Error e where

    displayError :: e -> String

instance Error () where
    displayError () = ""


-- | The preferred approach is to use 'Except' or 'ExceptT', but you can if
-- necessary use IO exceptions.
--
throwErrorAsException :: Error e => e -> IO a
throwErrorAsException e = throwIO (ErrorAsException e)

data ErrorAsException where
     ErrorAsException :: Error e => e -> ErrorAsException

instance Show ErrorAsException where
    show (ErrorAsException e) = show e

instance Exception ErrorAsException where
    displayException (ErrorAsException e) = displayError e


data FileError e = FileError   FilePath e
                 | FileErrorTempFile
                     FilePath
                     -- ^ Target path
                     FilePath
                     -- ^ Temporary path
                     Handle
                 | FileIOError FilePath IOException
  deriving Show

instance Error e => Error (FileError e) where
  displayError (FileErrorTempFile targetPath tempPath h)=
    "Error creating temporary file at: " ++ tempPath ++
    "/n" ++ "Target path: " ++ targetPath ++
    "/n" ++ "Handle: " ++ show h
  displayError (FileIOError path ioe) =
    path ++ ": " ++ displayException ioe
  displayError (FileError path e) =
    path ++ ": " ++ displayError e

instance Error IOException where
  displayError = show



--- WARNING: STUB for Bech32

class (HasTypeProxy a, SerialiseAsRawBytes a) => SerialiseAsBech32 a where

    -- | The human readable prefix to use when encoding this value to Bech32.
    --
    bech32PrefixFor :: a -> Text

    -- | The set of human readable prefixes that can be used for this type.
    --
    bech32PrefixesPermitted :: AsType a -> [Text]


-- serialiseToBech32 :: SerialiseAsBech32 a => a -> Text
serialiseToBech32 :: a -> Text
serialiseToBech32 _ = error "serialiseToBech32: stub not implemented"

-- deserialiseFromBech32 :: SerialiseAsBech32 a => AsType a -> Text -> Either Bech32DecodeError a
deserialiseFromBech32 :: AsType a -> Text -> Either Bech32DecodeError a
deserialiseFromBech32 _ _ = error "deserialiseFromBech32: stub not implemented"

data Bech32DecodeError

instance Show Bech32DecodeError where
    show = const "Bech32DecodeError: stub not implemented"

instance Error Bech32DecodeError where
    displayError = show
