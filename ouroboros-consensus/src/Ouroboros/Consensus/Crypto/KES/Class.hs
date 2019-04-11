{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Abstract key evolving signatures.
module Ouroboros.Consensus.Crypto.KES.Class
    ( KESAlgorithm (..)
    , SignedKES (..)
    , signedKES
    , verifySignedKES
    ) where

import           Codec.Serialise.Encoding (Encoding)
import           Codec.CBOR.Decoding (Decoder)
import           GHC.Generics (Generic)
import           Numeric.Natural (Natural)

import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Random

class ( Show (VerKeyKES v)
      , Ord (VerKeyKES v)
      , Show (SignKeyKES v)
      , Ord (SignKeyKES v)
      , Show (SigKES v)
      , Condense (SigKES v)
      , Ord (SigKES v)
      )
      => KESAlgorithm v where

    data VerKeyKES v :: *
    data SignKeyKES v :: *
    data SigKES v :: *

    encodeVerKeyKES :: VerKeyKES v -> Encoding
    decodeVerKeyKES :: Decoder s (VerKeyKES v)
    encodeSignKeyKES :: SignKeyKES v -> Encoding
    decodeSignKeyKES :: Decoder s (SignKeyKES v)
    encodeSigKES :: SigKES v -> Encoding
    decodeSigKES :: Decoder s (SigKES v)

    genKeyKES :: MonadRandom m => Natural -> m (SignKeyKES v)
    deriveVerKeyKES :: SignKeyKES v -> VerKeyKES v
    signKES :: (MonadRandom m)
            => (a -> Encoding)
            -> Natural
            -> a
            -> SignKeyKES v
            -> m (Maybe (SigKES v, SignKeyKES v))
    verifyKES :: (a -> Encoding) -> VerKeyKES v -> Natural -> a -> SigKES v -> Bool

newtype SignedKES v a = SignedKES {getSig :: SigKES v}
  deriving (Generic)

deriving instance KESAlgorithm v => Show (SignedKES v a)
deriving instance KESAlgorithm v => Eq   (SignedKES v a)
deriving instance KESAlgorithm v => Ord  (SignedKES v a)

instance Condense (SigKES v) => Condense (SignedKES v a) where
    condense (SignedKES sig) = condense sig

signedKES :: (KESAlgorithm v, MonadRandom m)
          => (a -> Encoding) -> Natural -> a -> SignKeyKES v -> m (Maybe (SignedKES v a, SignKeyKES v))
signedKES toEnc time a key = do
    m <- signKES toEnc time a key
    return $ case m of
        Nothing          -> Nothing
        Just (sig, key') -> Just (SignedKES sig, key')

verifySignedKES :: (KESAlgorithm v)
                => (a -> Encoding) -> VerKeyKES v -> Natural -> a -> SignedKES v a -> Bool
verifySignedKES toEnc vk j a (SignedKES sig) = verifyKES toEnc vk j a sig
