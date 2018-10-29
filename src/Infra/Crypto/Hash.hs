{-# LANGUAGE DeriveGeneric #-}

-- | Implementation of hashing functionality.
module Infra.Crypto.Hash
    ( hashBytes
    , Hash
    , hash
    , fromHash
    ) where

import qualified Crypto.Hash            as H
import           Crypto.Hash.Algorithms (MD5)
import qualified Data.ByteArray         as BA
import qualified Data.ByteString        as SB
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8  as SB8
import qualified Data.ByteString.Lazy   as LB
import           Data.List              (foldl')
import           Data.Maybe             (fromJust)
import           Data.String            (IsString (..))
import           GHC.Generics           (Generic)
import           Numeric.Natural
import           Serialise
import           Test.QuickCheck        (Arbitrary (..), Gen)

import           Infra.Util

type HashAlgorithm = MD5

hashBytes :: Natural
hashBytes = 16

newtype Hash a = Hash {getHash :: H.Digest HashAlgorithm}
    deriving (Show, Eq, Ord, Generic)

instance Condense (Hash a) where
    condense = show . getHash

instance Serialise (Hash a) where

    encode = encode . convert . getHash

    decode = do
        bs <- decodeBytes
        case H.digestFromByteString bs of
            Nothing -> fail "not a digest"
            Just d  -> return $ Hash d

instance IsString (Hash a) where
    fromString = Hash . fromJust . H.digestFromByteString . fst . B16.decode . SB8.pack

instance Arbitrary (Hash a) where

    -- It would be better to use add Constraints Serialise a and Arbitrary a,
    -- then generate arbitrary hashes by hashing arbitrary values of type a.
    arbitrary = do
        n <- arbitrary :: Gen Int
        let Hash h = hash n
        return $ Hash h

    shrink = const []

hash :: Serialise a => a -> Hash a
hash = Hash . H.hash . LB.toStrict . toLazyByteString . encode

convert :: H.Digest HashAlgorithm -> SB.ByteString
convert = BA.convert

fromHash :: Hash a -> Natural
fromHash = foldl' f 0 . SB.unpack . convert . getHash
  where
    f n b = n * 256 + fromIntegral b


