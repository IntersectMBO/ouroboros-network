{-# LANGUAGE ScopedTypeVariables #-}

-- DUPLICATE -- adapted from: cardano-api/src/Cardano/Api/SerialiseUsing.hs

-- | Raw binary serialisation
--
module Cardano.Api.SerialiseUsing (
    UsingRawBytes (..)
  , UsingRawBytesHex (..)
  ) where

import           Cardano.Api.Any
import           Data.Aeson.Types (FromJSON, FromJSONKey, ToJSON (..),
                     ToJSONKey)
import qualified Data.Aeson.Types as Aeson
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BSC
import           Data.String (IsString (..))
import qualified Data.Text.Encoding as Text
import           Data.Typeable (Typeable, tyConName, typeRep, typeRepTyCon)


-- | For use with @deriving via@, to provide 'ToCBOR' and 'FromCBOR' instances,
-- based on the 'SerialiseAsRawBytes' instance. Eg:
--
-- > deriving (ToCBOR, FromCBOR) via (UsingRawBytes Blah)
--
newtype UsingRawBytes a = UsingRawBytes a

instance (SerialiseAsRawBytes a, Typeable a) => ToCBOR (UsingRawBytes a) where
    toCBOR (UsingRawBytes x) = toCBOR (serialiseToRawBytes x)

instance (SerialiseAsRawBytes a, Typeable a) => FromCBOR (UsingRawBytes a) where
    fromCBOR = do
      bs <- fromCBOR
      case deserialiseFromRawBytes ttoken bs of
        Just x  -> return (UsingRawBytes x)
        Nothing -> fail ("cannot deserialise as a " ++ tname)
      where
        ttoken = proxyToAsType (Proxy :: Proxy a)
        tname  = (tyConName . typeRepTyCon . typeRep) (Proxy :: Proxy a)

instance (SerialiseAsRawBytes a, Typeable a) => EncCBOR (UsingRawBytes a)

instance (SerialiseAsRawBytes a, Typeable a) => DecCBOR (UsingRawBytes a)


-- | For use with @deriving via@, to provide instances for any\/all of 'Show',
-- 'IsString', 'ToJSON', 'FromJSON', 'ToJSONKey', FromJSONKey' using a hex
-- encoding, based on the 'SerialiseAsRawBytes' instance.
--
-- > deriving (Show, IsString) via (UsingRawBytesHex Blah)
-- > deriving (ToJSON, FromJSON) via (UsingRawBytesHex Blah)
-- > deriving (ToJSONKey, FromJSONKey) via (UsingRawBytesHex Blah)
--
newtype UsingRawBytesHex a = UsingRawBytesHex a

instance SerialiseAsRawBytes a => Show (UsingRawBytesHex a) where
    show (UsingRawBytesHex x) = show (serialiseToRawBytesHex x)

instance SerialiseAsRawBytes a => IsString (UsingRawBytesHex a) where
    fromString = either error id . deserialiseFromRawBytesBase16 . BSC.pack

instance SerialiseAsRawBytes a => ToJSON (UsingRawBytesHex a) where
    toJSON (UsingRawBytesHex x) = toJSON (serialiseToRawBytesHexText x)

instance (SerialiseAsRawBytes a, Typeable a) => FromJSON (UsingRawBytesHex a) where
  parseJSON =
    Aeson.withText tname $
      either fail pure . deserialiseFromRawBytesBase16 . Text.encodeUtf8
    where
      tname  = (tyConName . typeRepTyCon . typeRep) (Proxy :: Proxy a)

instance SerialiseAsRawBytes a => ToJSONKey (UsingRawBytesHex a) where
  toJSONKey =
    Aeson.toJSONKeyText $ \(UsingRawBytesHex x) -> serialiseToRawBytesHexText x

instance
  (SerialiseAsRawBytes a, Typeable a) => FromJSONKey (UsingRawBytesHex a) where

  fromJSONKey =
    Aeson.FromJSONKeyTextParser $
    either fail pure . deserialiseFromRawBytesBase16 . Text.encodeUtf8

deserialiseFromRawBytesBase16 ::
  SerialiseAsRawBytes a => ByteString -> Either String (UsingRawBytesHex a)
deserialiseFromRawBytesBase16 str =
  case Base16.decode str of
    Right raw -> case deserialiseFromRawBytes ttoken raw of
      Just x  -> Right (UsingRawBytesHex x)
      Nothing -> Left ("cannot deserialise " ++ show str)
    Left msg  -> Left ("invalid hex " ++ show str ++ ", " ++ msg)
  where
    ttoken = proxyToAsType (Proxy :: Proxy a)
