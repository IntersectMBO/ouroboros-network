{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

-- | Mock implementations of verifiable random functions.
module Ouroboros.Consensus.Crypto.VRF.Mock
  ( MockVRF
  ) where

import           Data.Proxy (Proxy (..))
import           GHC.Generics (Generic)
import           Numeric.Natural (Natural)

import           Ouroboros.Consensus.Crypto.Hash
import           Ouroboros.Consensus.Crypto.VRF.Class
import           Ouroboros.Consensus.Util.HList
import           Ouroboros.Consensus.Util.Random
import           Ouroboros.Network.Serialise

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

    evalVRF a sk = return $ evalVRF' a sk

    verifyVRF (VerKeyMockVRF n) a c = evalVRF' a (SignKeyMockVRF n) == c

evalVRF' :: Serialise a => a -> SignKeyVRF MockVRF -> (Natural, CertVRF MockVRF)
evalVRF' a sk@(SignKeyMockVRF n) =
    let y = fromHash $ hash @MD5 $ a :* sk :* Nil
    in  (y, CertMockVRF n)

instance Serialise (VerKeyVRF MockVRF)
instance Serialise (SignKeyVRF MockVRF)
instance Serialise (CertVRF MockVRF)
