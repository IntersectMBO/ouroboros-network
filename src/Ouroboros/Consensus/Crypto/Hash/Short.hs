{-# LANGUAGE PackageImports   #-}
{-# LANGUAGE TypeApplications #-}

-- | Implementation of short hashing algorithm, suitable for testing as
-- it's not very collision-resistant.
module Ouroboros.Consensus.Crypto.Hash.Short
    ( ShortHash
    ) where

import qualified "cryptonite" Crypto.Hash as H
import qualified Data.ByteArray as BA
import qualified Data.ByteString as B

import           Ouroboros.Consensus.Crypto.Hash.Class

data ShortHash

instance HashAlgorithm ShortHash where
    byteCount _ = 4
    digest p = B.take (fromIntegral $ byteCount p)
             . BA.convert @(H.Digest H.MD5) -- Internally, treat it like MD5.
             . H.hash
