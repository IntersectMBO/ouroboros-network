{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

-- | Mock implementations of verifiable random functions.
module Ouroboros.Consensus.Crypto.VRF.Mock
  ( MockVRF
  , VerKeyVRF (..)
  , SignKeyVRF (..)
  ) where

import           Codec.Serialise (Serialise(..))
import           Codec.Serialise.Encoding (Encoding)
import           Data.Proxy (Proxy (..))
import           GHC.Generics (Generic)
import           Numeric.Natural (Natural)

import           Ouroboros.Consensus.Crypto.Hash
import           Ouroboros.Consensus.Crypto.VRF.Class
import           Ouroboros.Consensus.Util.HList
import           Ouroboros.Consensus.Util.Random

data MockVRF

instance VRFAlgorithm MockVRF where

    newtype VerKeyVRF MockVRF = VerKeyMockVRF Int
        deriving (Show, Eq, Ord, Generic)

    newtype SignKeyVRF MockVRF = SignKeyMockVRF Int
        deriving (Show, Eq, Ord, Generic)

    newtype CertVRF MockVRF = CertMockVRF Int
        deriving (Show, Eq, Ord, Generic)

    maxVRF _ = 2 ^ (8 * byteCount (Proxy :: Proxy MD5)) - 1

    genKeyVRF = SignKeyMockVRF <$> nonNegIntR

    deriveVerKeyVRF (SignKeyMockVRF n) = VerKeyMockVRF n

    evalVRF toEnc a sk = return $ evalVRF' toEnc a sk

    verifyVRF toEnc (VerKeyMockVRF n) a c = evalVRF' toEnc a (SignKeyMockVRF n) == c

evalVRF' :: (a -> Encoding) -> a -> SignKeyVRF MockVRF -> (Natural, CertVRF MockVRF)
evalVRF' toEnc a sk@(SignKeyMockVRF n) =
    let y = fromHash $ hashWithSerialiser @MD5 id $ (toEnc a) <> (encode $ sk :* Nil)
    in  (y, CertMockVRF n)

instance Serialise (VerKeyVRF MockVRF)
instance Serialise (SignKeyVRF MockVRF)
instance Serialise (CertVRF MockVRF)
