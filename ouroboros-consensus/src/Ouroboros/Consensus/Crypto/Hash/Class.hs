{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

-- | Abstract hashing functionality.
module Ouroboros.Consensus.Crypto.Hash.Class
    ( HashAlgorithm (..)
    , ByteString
    , Hash
    , getHash
    , hash
    , hashWithSerialiser
    , fromHash
    ) where

import           Codec.Serialise.Encoding (Encoding)
import           Codec.CBOR.Decoding (decodeBytes)
import           Codec.CBOR.Write (toLazyByteString)
import           Codec.Serialise (Serialise (..))
import           Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as SB8
import qualified Data.ByteString.Lazy as LB
import           Data.List (foldl')
import           Data.Proxy (Proxy (..))
import           Data.String (IsString (..))
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           Numeric.Natural

import           Ouroboros.Consensus.Util.Condense

class Typeable h => HashAlgorithm h where
    byteCount :: proxy h -> Natural
    digest :: proxy h -> ByteString -> ByteString

newtype Hash h a = Hash {getHash :: ByteString}
    deriving (Eq, Ord, Generic, Typeable)

instance Condense (Hash h a) where
    condense = show

instance Show (Hash h a) where
    show = SB8.unpack . B16.encode . getHash

instance IsString (Hash h a) where
    fromString = Hash . fst . B16.decode . SB8.pack

instance HashAlgorithm h => Serialise (Hash h a) where

    encode = encode . getHash

    decode = do
        bs <- decodeBytes
        let la = SB.length bs
            le = fromIntegral $ byteCount (Proxy :: Proxy h)
        if la == le
            then return $ Hash bs
            else fail $ "expected " ++ show le ++ " byte(s), but got " ++ show la

hash :: forall h a. (HashAlgorithm h, Serialise a) => a -> Hash h a
hash = hashWithSerialiser encode

hashWithSerialiser :: forall h a. HashAlgorithm h => (a -> Encoding) -> a -> Hash h a
hashWithSerialiser toEnc = Hash . digest (Proxy :: Proxy h)  . LB.toStrict . toLazyByteString . toEnc

fromHash :: Hash h a -> Natural
fromHash = foldl' f 0 . SB.unpack . getHash
  where
    f n b = n * 256 + fromIntegral b
