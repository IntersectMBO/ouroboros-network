{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Abstract digital signatures.
module Ouroboros.Consensus.Crypto.DSIGN.Class
    ( DSIGNAlgorithm (..)
      -- TODO: Added on top of Lars' stuff
    , Signed
    , signed
    ) where

import           Crypto.Random (MonadRandom)
import           GHC.Generics (Generic)

import           Ouroboros.Network.Serialise

class ( Show (VerKeyDSIGN v)
      , Ord (VerKeyDSIGN v)
      , Serialise (VerKeyDSIGN v)
      , Show (SignKeyDSIGN v)
      , Ord (SignKeyDSIGN v)
      , Serialise (SignKeyDSIGN v)
      , Show (SigDSIGN v)
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

newtype Signed v a = Signed (SigDSIGN v)
  deriving (Generic)

deriving instance DSIGNAlgorithm v => Show (Signed v a)
deriving instance DSIGNAlgorithm v => Eq   (Signed v a)
deriving instance DSIGNAlgorithm v => Ord  (Signed v a)

instance DSIGNAlgorithm v => Serialise (Signed v a) where
  -- use Generic instance

signed :: (DSIGNAlgorithm v, MonadRandom m, Serialise a)
       => a -> SignKeyDSIGN v -> m (Signed v a)
signed a key = Signed <$> signDSIGN a key
