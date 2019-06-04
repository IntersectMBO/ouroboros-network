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
    , encodeSignedKES
    , decodeSignedKES
    ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.Serialise.Encoding (Encoding)
import           GHC.Exts (Constraint)
import           GHC.Generics (Generic)
import           Numeric.Natural (Natural)

import           Ouroboros.Consensus.Util (Empty)
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

    type Signable v :: * -> Constraint
    type Signable c = Empty

    encodeVerKeyKES  :: VerKeyKES v -> Encoding
    encodeSignKeyKES :: SignKeyKES v -> Encoding
    encodeSigKES     :: SigKES v -> Encoding

    decodeVerKeyKES  :: Decoder s (VerKeyKES v)
    decodeSignKeyKES :: Decoder s (SignKeyKES v)
    decodeSigKES     :: Decoder s (SigKES v)

    genKeyKES :: MonadRandom m => Natural -> m (SignKeyKES v)
    deriveVerKeyKES :: SignKeyKES v -> VerKeyKES v
    signKES :: (MonadRandom m, Signable v a)
            => (a -> Encoding)
            -> Natural
            -> a
            -> SignKeyKES v
            -> m (Maybe (SigKES v, SignKeyKES v))
    verifyKES :: Signable v a
              => (a -> Encoding)
              -> VerKeyKES v -> Natural -> a -> SigKES v -> Either String ()

newtype SignedKES v a = SignedKES {getSig :: SigKES v}
  deriving (Generic)

deriving instance KESAlgorithm v => Show (SignedKES v a)
deriving instance KESAlgorithm v => Eq   (SignedKES v a)
deriving instance KESAlgorithm v => Ord  (SignedKES v a)

instance Condense (SigKES v) => Condense (SignedKES v a) where
    condense (SignedKES sig) = condense sig

signedKES :: (KESAlgorithm v, MonadRandom m, Signable v a)
          => (a -> Encoding) -> Natural -> a -> SignKeyKES v -> m (Maybe (SignedKES v a, SignKeyKES v))
signedKES toEnc time a key = do
    m <- signKES toEnc time a key
    return $ case m of
        Nothing          -> Nothing
        Just (sig, key') -> Just (SignedKES sig, key')

verifySignedKES :: (KESAlgorithm v, Signable v a)
                => (a -> Encoding)
                -> VerKeyKES v -> Natural -> a -> SignedKES v a -> Either String ()
verifySignedKES toEnc vk j a (SignedKES sig) = verifyKES toEnc vk j a sig

encodeSignedKES :: KESAlgorithm v => SignedKES v a -> Encoding
encodeSignedKES (SignedKES s) = encodeSigKES s

decodeSignedKES :: KESAlgorithm v => Decoder s (SignedKES v a)
decodeSignedKES = SignedKES <$> decodeSigKES
