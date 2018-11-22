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

import           GHC.Generics (Generic)
import           Numeric.Natural (Natural)

import           Ouroboros.Consensus.Util (Condense (..))
import           Ouroboros.Consensus.Util.Random
import           Ouroboros.Network.Serialise

class ( Show (VerKeyKES v)
      , Ord (VerKeyKES v)
      , Serialise (VerKeyKES v)
      , Show (SignKeyKES v)
      , Ord (SignKeyKES v)
      , Serialise (SignKeyKES v)
      , Show (SigKES v)
      , Condense (SigKES v)
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

newtype SignedKES v a = SignedKES {getSig :: SigKES v}
  deriving (Generic)

deriving instance KESAlgorithm v => Show (SignedKES v a)
deriving instance KESAlgorithm v => Eq   (SignedKES v a)
deriving instance KESAlgorithm v => Ord  (SignedKES v a)

instance Condense (SigKES v) => Condense (SignedKES v a) where
    condense (SignedKES sig) = condense sig

instance KESAlgorithm v => Serialise (SignedKES v a) where
  -- use Generic instance

signedKES :: (KESAlgorithm v, MonadRandom m, Serialise a)
          => Natural -> a -> SignKeyKES v -> m (Maybe (SignedKES v a, SignKeyKES v))
signedKES time a key = do
    m <- signKES time a key
    return $ case m of
        Nothing          -> Nothing
        Just (sig, key') -> Just (SignedKES sig, key')

verifySignedKES :: (KESAlgorithm v, Serialise a)
                => VerKeyKES v -> Natural -> a -> SignedKES v a -> Bool
verifySignedKES vk j a (SignedKES sig) = verifyKES vk j a sig
