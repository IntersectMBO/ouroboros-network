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

import           Codec.Serialise (Serialise)
import           GHC.Generics (Generic)

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

    genKeyDSIGN = SignKeyMockDSIGN <$> nonNegIntR

    deriveVerKeyDSIGN (SignKeyMockDSIGN n) = VerKeyMockDSIGN n

    signDSIGN a sk = return $ mockSign a sk

    verifyDSIGN (VerKeyMockDSIGN n) a s = s == mockSign a (SignKeyMockDSIGN n)

mockSign :: Serialise a => a -> SignKeyDSIGN MockDSIGN -> SigDSIGN MockDSIGN
mockSign a (SignKeyMockDSIGN n) = SigMockDSIGN (getHash $ hash @ShortHash a) n

instance Condense (SigDSIGN MockDSIGN) where
    condense (SigMockDSIGN _ i) = show i

instance Serialise (VerKeyDSIGN MockDSIGN)
instance Serialise (SignKeyDSIGN MockDSIGN)
instance Serialise (SigDSIGN MockDSIGN)

-- | Get the id of the signer from a signature. Used for testing.
verKeyIdFromSigned :: SignedDSIGN MockDSIGN a -> Int
verKeyIdFromSigned (SignedDSIGN (SigMockDSIGN _ i)) = i
