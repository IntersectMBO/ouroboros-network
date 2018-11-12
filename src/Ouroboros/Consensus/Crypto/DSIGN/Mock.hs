{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

-- | Mock implementation of digital signatures.
module Ouroboros.Consensus.Crypto.DSIGN.Mock
    ( MockDSIGN
    , SignKeyDSIGN(..)
    , VerKeyDSIGN(..)
    ) where

import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as SB8
import           GHC.Generics (Generic)

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

    data SigDSIGN MockDSIGN = SigMockDSIGN ByteString Int
        deriving (Show, Eq, Ord, Generic)

    genKeyDSIGN = SignKeyMockDSIGN <$> nonNegIntR

    deriveVerKeyDSIGN (SignKeyMockDSIGN n) = VerKeyMockDSIGN n

    signDSIGN a sk = return $ mkSig a sk

    verifyDSIGN (VerKeyMockDSIGN n) a s = s == mkSig a (SignKeyMockDSIGN n)

mkSig :: Serialise a => a -> SignKeyDSIGN MockDSIGN -> SigDSIGN MockDSIGN
mkSig a (SignKeyMockDSIGN n) = SigMockDSIGN (getHash $ hash @ShortHash a) n

instance Condense (SigDSIGN MockDSIGN) where
    condense (SigMockDSIGN n i) = (SB8.unpack . B16.encode $ n)
                                <> ":"
                                <> show i

instance Serialise (VerKeyDSIGN MockDSIGN)
instance Serialise (SignKeyDSIGN MockDSIGN)
instance Serialise (SigDSIGN MockDSIGN)
