{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

-- | Mock key evolving signatures.
module Ouroboros.Consensus.Crypto.KES.Simple
    ( SimpleKES
    ) where

import           Control.Monad (replicateM)
import           Data.Vector (Vector, fromList, (!?))
import           GHC.Generics (Generic)
import           Numeric.Natural (Natural)

import           Ouroboros.Consensus.Crypto.DSIGN
import           Ouroboros.Consensus.Crypto.KES.Class
import           Ouroboros.Consensus.Util (Condense (..))
import           Ouroboros.Network.Serialise (Serialise)

data SimpleKES d

instance DSIGNAlgorithm d => KESAlgorithm (SimpleKES d) where

    newtype VerKeyKES (SimpleKES d) = VerKeySimpleKES (Vector (VerKeyDSIGN d))
        deriving Generic

    newtype SignKeyKES (SimpleKES d) =
        SignKeySimpleKES ([VerKeyDSIGN d], [(Natural, SignKeyDSIGN d)])
        deriving Generic

    newtype SigKES (SimpleKES d) = SigSimpleKES (SigDSIGN d)
        deriving Generic

    genKeyKES duration = do
        sks <- replicateM (fromIntegral duration) genKeyDSIGN
        let vks = map deriveVerKeyDSIGN sks
        return $ SignKeySimpleKES (vks, zip [0..] sks)

    deriveVerKeyKES (SignKeySimpleKES (vks, _)) = VerKeySimpleKES $ fromList vks

    signKES j a (SignKeySimpleKES (vks, xs)) = case dropWhile (\(k, _) -> k < j) xs of
        []           -> return Nothing
        (_, sk) : ys -> do
            sig <- signDSIGN a sk
            return $ Just (SigSimpleKES sig, SignKeySimpleKES (vks, ys))

    verifyKES (VerKeySimpleKES vks) j a (SigSimpleKES sig) =
        case vks !? fromIntegral j of
            Nothing -> False
            Just vk -> verifyDSIGN vk a sig

deriving instance DSIGNAlgorithm d => Show (VerKeyKES (SimpleKES d))
deriving instance DSIGNAlgorithm d => Eq (VerKeyKES (SimpleKES d))
deriving instance DSIGNAlgorithm d => Ord (VerKeyKES (SimpleKES d))
deriving instance DSIGNAlgorithm d => Serialise (VerKeyKES (SimpleKES d))

deriving instance DSIGNAlgorithm d => Show (SignKeyKES (SimpleKES d))
deriving instance DSIGNAlgorithm d => Eq (SignKeyKES (SimpleKES d))
deriving instance DSIGNAlgorithm d => Ord (SignKeyKES (SimpleKES d))
deriving instance DSIGNAlgorithm d => Serialise (SignKeyKES (SimpleKES d))

deriving instance DSIGNAlgorithm d => Show (SigKES (SimpleKES d))
deriving instance DSIGNAlgorithm d => Eq (SigKES (SimpleKES d))
deriving instance DSIGNAlgorithm d => Ord (SigKES (SimpleKES d))
deriving instance DSIGNAlgorithm d => Serialise (SigKES (SimpleKES d))

instance Condense (SigDSIGN d) => Condense (SigKES (SimpleKES d)) where
    condense (SigSimpleKES sig) = condense sig
