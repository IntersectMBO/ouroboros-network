{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

-- | Mock key evolving signatures.
module Ouroboros.Consensus.Crypto.KES.Mock
    ( MockKES
    , VerKeyKES(..)
    , SignKeyKES(..)
    ) where

import           Codec.Serialise (Serialise(..))
import           GHC.Generics (Generic)
import           Numeric.Natural (Natural)

import           Ouroboros.Consensus.Crypto.Hash
import           Ouroboros.Consensus.Crypto.KES.Class
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Random

data MockKES

type H = MD5

instance KESAlgorithm MockKES where

    newtype VerKeyKES MockKES = VerKeyMockKES Int
        deriving (Show, Eq, Ord, Generic, Serialise)

    newtype SignKeyKES MockKES = SignKeyMockKES (VerKeyKES MockKES, Natural, Natural)
        deriving (Show, Eq, Ord, Generic, Serialise)

    data SigKES MockKES = SigMockKES Natural (SignKeyKES MockKES)
        deriving (Show, Eq, Ord, Generic)

    encodeVerKeyKES = encode
    encodeSignKeyKES = encode
    encodeSigKES = encode

    decodeSignKeyKES = decode
    decodeVerKeyKES = decode
    decodeSigKES = decode

    genKeyKES duration = do
        vk <- VerKeyMockKES <$> nonNegIntR
        return $ SignKeyMockKES (vk, 0, duration)

    deriveVerKeyKES (SignKeyMockKES (vk, _, _)) = vk

    signKES toEnc j a (SignKeyMockKES (vk, k, t))
        | j >= k && j < t = return $ Just
            ( SigMockKES (fromHash $ hashWithSerialiser @H toEnc a) (SignKeyMockKES (vk, j, t))
            , SignKeyMockKES (vk, j + 1, t)
            )
        | otherwise       = return Nothing

    verifyKES toEnc vk j a (SigMockKES h (SignKeyMockKES (vk', j', _))) =
        j == j' &&
        vk == vk' &&
        fromHash (hashWithSerialiser @H toEnc a) == h

instance Serialise (SigKES MockKES) where

instance Condense (SigKES MockKES) where
    condense (SigMockKES n (SignKeyMockKES (VerKeyMockKES v, j, d))) =
           show n
        <> ":"
        <> show v
        <> ":"
        <> show j
        <> ":"
        <> show d
