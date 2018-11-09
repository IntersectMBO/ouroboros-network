{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Abstract key evolving signatures.
module Ouroboros.Consensus.Crypto.KES.Class
    ( KESAlgorithm (..)
    ) where

import           Crypto.Random (MonadRandom)
import           Numeric.Natural (Natural)

import           Ouroboros.Network.Serialise

class ( Show (VerKeyKES v)
      , Ord (VerKeyKES v)
      , Serialise (VerKeyKES v)
      , Show (SignKeyKES v)
      , Ord (SignKeyKES v)
      , Serialise (SignKeyKES v)
      , Show (SigKES v)
      , Ord (SigKES v)
      , Serialise (SigKES v)
      )
      => KESAlgorithm v where

    data VerKeyKES v :: *
    data SignKeyKES v :: *
    data SigKES v :: *

    genKeyKES :: MonadRandom m => Natural -> m (SignKeyKES v)
    deriveVerKeyKES :: SignKeyKES v -> VerKeyKES v
    signKES :: (MonadRandom m, Serialise a)
            => Natural
            -> a
            -> SignKeyKES v
            -> m (Maybe (SigKES v, SignKeyKES v))
    verifyKES :: Serialise a => VerKeyKES v -> Natural -> a -> SigKES v -> Bool
