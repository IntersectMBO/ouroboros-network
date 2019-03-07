{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Abstract digital signatures.
module Ouroboros.Consensus.Crypto.DSIGN.Class
    ( DSIGNAlgorithm (..)
    , SignedDSIGN (..)
    , signedDSIGN
    , verifySignedDSIGN
    ) where

import           Codec.Serialise (Serialise (..))
import           Crypto.Random (MonadRandom)
import           GHC.Generics (Generic)

import           Ouroboros.Consensus.Util.Condense

class ( Show (VerKeyDSIGN v)
      , Ord (VerKeyDSIGN v)
      , Serialise (VerKeyDSIGN v)
      , Show (SignKeyDSIGN v)
      , Ord (SignKeyDSIGN v)
      , Serialise (SignKeyDSIGN v)
      , Show (SigDSIGN v)
      , Condense (SigDSIGN v)
      , Ord (SigDSIGN v)
      , Serialise (SigDSIGN v)
      )
      => DSIGNAlgorithm v where

    data VerKeyDSIGN v :: *
    data SignKeyDSIGN v :: *
    data SigDSIGN v :: *

    genKeyDSIGN :: MonadRandom m => m (SignKeyDSIGN v)
    deriveVerKeyDSIGN :: SignKeyDSIGN v -> VerKeyDSIGN v
    signDSIGN :: (MonadRandom m, Serialise a) => a -> SignKeyDSIGN v -> m (SigDSIGN v)
    verifyDSIGN :: Serialise a => VerKeyDSIGN v -> a -> SigDSIGN v -> Bool

newtype SignedDSIGN v a = SignedDSIGN (SigDSIGN v)
  deriving (Generic)

deriving instance DSIGNAlgorithm v => Show (SignedDSIGN v a)
deriving instance DSIGNAlgorithm v => Eq   (SignedDSIGN v a)
deriving instance DSIGNAlgorithm v => Ord  (SignedDSIGN v a)

instance Condense (SigDSIGN v) => Condense (SignedDSIGN v a) where
    condense (SignedDSIGN sig) = condense sig

instance DSIGNAlgorithm v => Serialise (SignedDSIGN v a) where
  -- use Generic instance
  --
signedDSIGN :: (DSIGNAlgorithm v, MonadRandom m, Serialise a)
            => a -> SignKeyDSIGN v -> m (SignedDSIGN v a)
signedDSIGN a key = SignedDSIGN <$> signDSIGN a key

verifySignedDSIGN :: (DSIGNAlgorithm v, Serialise a)
                  => VerKeyDSIGN v -> a -> SignedDSIGN v a -> Bool
verifySignedDSIGN key a (SignedDSIGN s) = verifyDSIGN key a s
