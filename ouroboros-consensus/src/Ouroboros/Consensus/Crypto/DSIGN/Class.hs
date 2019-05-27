{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Abstract digital signatures.
module Ouroboros.Consensus.Crypto.DSIGN.Class
    ( DSIGNAlgorithm (..)
    , SignedDSIGN (..)
    , Empty
    , signedDSIGN
    , verifySignedDSIGN
    , encodeSignedDSIGN
    , decodeSignedDSIGN
    ) where

import           Codec.Serialise.Encoding (Encoding)
import           Codec.CBOR.Decoding (Decoder)
import           Crypto.Random (MonadRandom)
import           GHC.Exts (Constraint)
import           GHC.Generics (Generic)
import           Ouroboros.Consensus.Util.Condense

class Empty a
instance Empty a

class ( Show (VerKeyDSIGN v)
      , Ord (VerKeyDSIGN v)
      , Show (SignKeyDSIGN v)
      , Ord (SignKeyDSIGN v)
      , Show (SigDSIGN v)
      , Condense (SigDSIGN v)
      , Ord (SigDSIGN v)
      )
      => DSIGNAlgorithm v where

    data VerKeyDSIGN v :: *
    data SignKeyDSIGN v :: *
    data SigDSIGN v :: *

    type Signable v :: * -> Constraint
    type Signable c = Empty

    encodeVerKeyDSIGN :: VerKeyDSIGN v -> Encoding
    decodeVerKeyDSIGN :: Decoder s (VerKeyDSIGN v)
    encodeSignKeyDSIGN :: SignKeyDSIGN v -> Encoding
    decodeSignKeyDSIGN :: Decoder s (SignKeyDSIGN v)
    encodeSigDSIGN :: SigDSIGN v -> Encoding
    decodeSigDSIGN :: Decoder s (SigDSIGN v)

    genKeyDSIGN :: MonadRandom m => m (SignKeyDSIGN v)
    deriveVerKeyDSIGN :: SignKeyDSIGN v -> VerKeyDSIGN v
    signDSIGN :: (MonadRandom m, Signable v a) => (a -> Encoding) -> a -> SignKeyDSIGN v -> m (SigDSIGN v)
    verifyDSIGN :: (Signable v a) => (a -> Encoding) -> VerKeyDSIGN v -> a -> SigDSIGN v -> Bool

newtype SignedDSIGN v a = SignedDSIGN (SigDSIGN v)
  deriving (Generic)

deriving instance DSIGNAlgorithm v => Show (SignedDSIGN v a)
deriving instance DSIGNAlgorithm v => Eq   (SignedDSIGN v a)
deriving instance DSIGNAlgorithm v => Ord  (SignedDSIGN v a)

instance Condense (SigDSIGN v) => Condense (SignedDSIGN v a) where
    condense (SignedDSIGN sig) = condense sig

signedDSIGN :: (DSIGNAlgorithm v, MonadRandom m, Signable v a)
            => (a -> Encoding) -> a -> SignKeyDSIGN v -> m (SignedDSIGN v a)
signedDSIGN encoder a key = SignedDSIGN <$> signDSIGN encoder a key

verifySignedDSIGN :: (DSIGNAlgorithm v, Signable v a)
                  => (a -> Encoding) -> VerKeyDSIGN v -> a -> SignedDSIGN v a -> Bool
verifySignedDSIGN encoder key a (SignedDSIGN s) = verifyDSIGN encoder key a s

encodeSignedDSIGN :: DSIGNAlgorithm v => SignedDSIGN v a -> Encoding
encodeSignedDSIGN (SignedDSIGN s) = encodeSigDSIGN s

decodeSignedDSIGN :: DSIGNAlgorithm v => Decoder s (SignedDSIGN v a)
decodeSignedDSIGN = SignedDSIGN <$> decodeSigDSIGN
