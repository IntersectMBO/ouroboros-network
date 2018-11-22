{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Abstract key evolving signatures.
module Ouroboros.Consensus.Crypto.KES.Class
    ( KESAlgorithm (..)
    , SignedKES
    , signedKES
    , Duration_Seed_SK (..)
    , Duration_Seed_SK_Times (..)
    ) where

import           GHC.Generics (Generic)
import           Numeric.Natural (Natural)
import           Test.QuickCheck (Arbitrary (..), Gen)

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

newtype SignedKES v a = SignedKES (SigKES v)
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

--
-- Testing
--

data Duration_Seed_SK v = Duration_Seed_SK Natural Seed (SignKeyKES v)
    deriving Generic

deriving instance KESAlgorithm v => Show (Duration_Seed_SK v)
deriving instance KESAlgorithm v => Eq (Duration_Seed_SK v)
deriving instance KESAlgorithm v => Serialise (Duration_Seed_SK v)

instance KESAlgorithm v => Arbitrary (Duration_Seed_SK v) where

    arbitrary = do
        duration <- genNat
        seed <- arbitrary
        return $ duration_Seed_SK duration seed

    shrink (Duration_Seed_SK duration seed _) =
        [duration_Seed_SK d seed | d <- shrinkNat duration]

instance KESAlgorithm v => Arbitrary (VerKeyKES v) where

    arbitrary = do
        Duration_Seed_SK _ _ sk <- arbitrary
        return $ deriveVerKeyKES sk

    shrink = const []

data Duration_Seed_SK_Times v a =
    Duration_Seed_SK_Times Natural Seed (SignKeyKES v) [(Natural, a)]
    deriving Generic

instance (KESAlgorithm v, Arbitrary a) => Arbitrary (Duration_Seed_SK_Times v a) where

    arbitrary = arbitrary >>= gen_Duration_Seed_SK_Times

    shrink (Duration_Seed_SK_Times d s sk ts) = do
        Duration_Seed_SK d' s' sk' <- shrink $ Duration_Seed_SK d s sk
        let ts' = filter ((< d') . fst) ts
        return $ Duration_Seed_SK_Times d' s' sk' ts'

deriving instance (KESAlgorithm v, Show a) => Show (Duration_Seed_SK_Times v a)
deriving instance (KESAlgorithm v, Eq a) => Eq (Duration_Seed_SK_Times v a)
deriving instance (KESAlgorithm v, Serialise a) => Serialise (Duration_Seed_SK_Times v a)

duration_Seed_SK :: KESAlgorithm v => Natural -> Seed -> Duration_Seed_SK v
duration_Seed_SK duration seed =
    let sk = withSeed seed $ genKeyKES duration
    in  Duration_Seed_SK duration seed sk

gen_Duration_Seed_SK_Times :: Arbitrary a
                           => Duration_Seed_SK v
                           -> Gen (Duration_Seed_SK_Times v a)
gen_Duration_Seed_SK_Times (Duration_Seed_SK duration seed sk) = do
    ts <- genTimes 0
    return $ Duration_Seed_SK_Times duration seed sk ts
  where
    genTimes :: Arbitrary a => Natural -> Gen [(Natural, a)]
    genTimes j
        | j >= duration = return []
        | otherwise = do
            k  <- genNatBetween j (duration - 1)
            a  <- arbitrary
            ns <- genTimes $ k + 1
            return ((k, a) : ns)
