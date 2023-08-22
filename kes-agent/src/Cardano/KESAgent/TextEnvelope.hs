{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.KESAgent.TextEnvelope
where

import Cardano.Binary ( FromCBOR (..), ToCBOR (..), serialize', decodeFull' )
import qualified Cardano.Binary as Binary
import qualified Formatting

import Control.Monad ( when, (<=<) )
import Data.Bifunctor ( first )
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson
  ( ToJSON (..)
  , FromJSON (..)
  , parseJSON
  , object
  , withObject
  , (.=)
  , (.:)
  )
import qualified Data.Aeson as JSON
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.ByteString.Base16 as Base16
import Data.Proxy ( Proxy (..) )

encodeTextEnvelopeFile :: HasTextEnvelope a => FilePath -> a -> IO ()
encodeTextEnvelopeFile path value = do
  JSON.encodeFile path $ toTextEnvelope value

decodeTextEnvelopeFile :: HasTextEnvelope a => FilePath -> IO (Either String a)
decodeTextEnvelopeFile path = do
  result <- JSON.eitherDecodeFileStrict' path
  return $ result >>= fromTextEnvelope

encodeTextEnvelope :: HasTextEnvelope a => a -> ByteString
encodeTextEnvelope = LBS.toStrict . JSON.encode . toTextEnvelope

decodeTextEnvelope :: HasTextEnvelope a => ByteString -> Either String a
decodeTextEnvelope = fromTextEnvelope <=< JSON.eitherDecodeStrict

data TextEnvelope =
  TextEnvelope
    { teType :: String
    , teDescription :: String
    , teRawCBOR :: ByteString
    }

instance ToJSON TextEnvelope where
  toJSON TextEnvelope {teType, teDescription, teRawCBOR} =
    object [ "type"        .= teType
           , "description" .= teDescription
           , "cborHex"     .= decodeUtf8 (Base16.encode teRawCBOR)
           ]

instance FromJSON TextEnvelope where
  parseJSON = withObject "TextEnvelope" $ \v ->
                TextEnvelope <$> (v .: "type")
                             <*> (v .: "description")
                             <*> (parseJSONBase16 =<< v .: "cborHex")
    where
      parseJSONBase16 v =
        either fail return . Base16.decode . encodeUtf8 =<< parseJSON v

class (ToCBOR a, FromCBOR a) => HasTextEnvelope a where
  getTEType :: proxy a -> String
  getTEDescription :: proxy a -> String

toTextEnvelope :: forall a. HasTextEnvelope a => a -> TextEnvelope
toTextEnvelope val =
  TextEnvelope
    (getTEType proxy)
    (getTEDescription proxy)
    (serialize' val)
  where
    proxy :: Proxy a
    proxy = Proxy

fromTextEnvelope :: forall a m. (HasTextEnvelope a) => TextEnvelope -> Either String a
fromTextEnvelope TextEnvelope { teType, teRawCBOR } = do
  when (teType /= getTEType proxy)
      (Left $
        "Unexpected text envelope type, expected " ++ getTEType proxy ++
        ", but found " ++ teType
      )
  first (Formatting.formatToString Formatting.build) $ decodeFull' teRawCBOR
  where
    proxy :: Proxy a
    proxy = Proxy
