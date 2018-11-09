{-# LANGUAGE PackageImports #-}

-- | Implementation of the MD5 hashing algorithm.
module Ouroboros.Consensus.Crypto.Hash.MD5
    ( MD5
    ) where

import qualified "cryptonite" Crypto.Hash as H
import qualified Data.ByteArray as BA

import           Ouroboros.Consensus.Crypto.Hash.Class

data MD5

instance HashAlgorithm MD5 where
    byteCount _ = 16
    digest _ = convert . H.hash

convert :: H.Digest H.MD5 -> ByteString
convert = BA.convert
