{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

-- | Mock implementation of digital signatures.
module Ouroboros.Consensus.Crypto.DSIGN.Mock
    ( MockDSIGN
    , SignKeyDSIGN(..)
    , VerKeyDSIGN(..)
    , verKeyIdFromSigned
    , mockSign
    ) where

import           Codec.CBOR.Encoding (Encoding)
import           Codec.Serialise (Serialise (..))
import           GHC.Generics (Generic)
import           GHC.Stack

import           Ouroboros.Consensus.Crypto.DSIGN.Class
import           Ouroboros.Consensus.Crypto.Hash
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Random

data MockDSIGN

instance DSIGNAlgorithm MockDSIGN where

    newtype VerKeyDSIGN MockDSIGN = VerKeyMockDSIGN Int
        deriving (Show, Eq, Ord, Generic)

    newtype SignKeyDSIGN MockDSIGN = SignKeyMockDSIGN Int
        deriving (Show, Eq, Ord, Generic)

    data SigDSIGN MockDSIGN = SigMockDSIGN ByteString Int
        deriving (Show, Eq, Ord, Generic)

    encodeVerKeyDSIGN  = encode
    encodeSignKeyDSIGN = encode
    encodeSigDSIGN     = encode

    decodeVerKeyDSIGN  = decode
    decodeSignKeyDSIGN = decode
    decodeSigDSIGN     = decode

    genKeyDSIGN = SignKeyMockDSIGN <$> nonNegIntR

    deriveVerKeyDSIGN (SignKeyMockDSIGN n) = VerKeyMockDSIGN n

    signDSIGN toEnc a sk = return $ mockSign toEnc a sk

    verifyDSIGN toEnc (VerKeyMockDSIGN n) a s =
      if s == mockSign toEnc a (SignKeyMockDSIGN n)
        then Right ()
        else Left $ show $ MockVerificationFailure {
                 vErrVerKey    = VerKeyMockDSIGN n
               , vErrSignature = s
               , vErrCallStack = prettyCallStack callStack
               }

-- | Debugging: provide information about the verification failure
--
-- We don't include the actual value here as that would require propagating a
-- 'Show' constraint.
data VerificationFailure = MockVerificationFailure {
      vErrVerKey    :: VerKeyDSIGN MockDSIGN
    , vErrSignature :: SigDSIGN    MockDSIGN
    , vErrCallStack :: String
    }
  deriving (Show)

mockSign :: (a -> Encoding) -> a -> SignKeyDSIGN MockDSIGN -> SigDSIGN MockDSIGN
mockSign toEnc a (SignKeyMockDSIGN n) = SigMockDSIGN (getHash $ hashWithSerialiser @ShortHash toEnc a) n

instance Condense (SigDSIGN MockDSIGN) where
    condense (SigMockDSIGN _ i) = show i

instance Serialise (VerKeyDSIGN MockDSIGN)
instance Serialise (SignKeyDSIGN MockDSIGN)
instance Serialise (SigDSIGN MockDSIGN)

-- | Get the id of the signer from a signature. Used for testing.
verKeyIdFromSigned :: SignedDSIGN MockDSIGN a -> Int
verKeyIdFromSigned (SignedDSIGN (SigMockDSIGN _ i)) = i
