{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

-- | Mock implementation of digital signatures.
module Ouroboros.Consensus.Crypto.DSIGN.Mock
    ( MockDSIGN
    , SignKeyDSIGN(..)
    ) where

import           GHC.Generics (Generic)
import           Numeric.Natural (Natural)

import           Ouroboros.Consensus.Crypto.DSIGN.Class
import           Ouroboros.Consensus.Crypto.Hash
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.Random
import           Ouroboros.Network.Serialise

data MockDSIGN

instance DSIGNAlgorithm MockDSIGN where

    newtype VerKeyDSIGN MockDSIGN = VerKeyMockDSIGN Int
        deriving (Show, Eq, Ord, Generic)

    newtype SignKeyDSIGN MockDSIGN = SignKeyMockDSIGN Int
        deriving (Show, Eq, Ord, Generic)

    data SigDSIGN MockDSIGN = SigMockDSIGN Natural Int
        deriving (Show, Eq, Ord, Generic)

    genKeyDSIGN = SignKeyMockDSIGN <$> nonNegIntR

    deriveVerKeyDSIGN (SignKeyMockDSIGN n) = VerKeyMockDSIGN n

    signDSIGN a sk = return $ mkSig a sk

    verifyDSIGN (VerKeyMockDSIGN n) a s = s == mkSig a (SignKeyMockDSIGN n)

mkSig :: Serialise a => a -> SignKeyDSIGN MockDSIGN -> SigDSIGN MockDSIGN
mkSig a (SignKeyMockDSIGN n) = SigMockDSIGN (fromHash $ hash @MD5 a) n

instance Condense (SigDSIGN MockDSIGN) where
    condense (SigMockDSIGN n i) = show n <> ":" <> show i

instance Serialise (VerKeyDSIGN MockDSIGN)
instance Serialise (SignKeyDSIGN MockDSIGN)
instance Serialise (SigDSIGN MockDSIGN)
