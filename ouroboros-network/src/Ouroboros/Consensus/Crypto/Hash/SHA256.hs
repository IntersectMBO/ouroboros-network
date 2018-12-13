{-# LANGUAGE PackageImports #-}

-- | Implementation of the SHA256 hashing algorithm.
module Ouroboros.Consensus.Crypto.Hash.SHA256
    ( SHA256
    ) where

import qualified "cryptonite" Crypto.Hash as H
import qualified Data.ByteArray as BA

import           Ouroboros.Consensus.Crypto.Hash.Class

data SHA256

instance HashAlgorithm SHA256 where
    byteCount _ = 32
    digest _ = convert . H.hash

convert :: H.Digest H.SHA256 -> ByteString
convert = BA.convert
