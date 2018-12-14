{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ouroboros.Consensus.Util.Random
    ( MonadRandom (..)
    , Seed (..)
    , nonNegIntR
    , genNatBetween
    , genNat
    , shrinkNat
    , withSeed
    , MonadPseudoRandomT -- opaque
    , withDRGT
    , seedToChaCha
    , generateElement
    )
    where

import           Control.Monad.State
import           Crypto.Number.Generate (generateBetween)
import           Crypto.Random (ChaChaDRG, DRG, MonadPseudoRandom,
                                MonadRandom (..), drgNewTest,
                                randomBytesGenerate, withDRG)
import           Data.ByteString (ByteString, unpack)
import           Data.List (genericLength)
import           Data.Word (Word64)
import           Numeric.Natural (Natural)
import           Test.QuickCheck

import           Ouroboros.Network.Serialise (Serialise)

nonNegIntR :: MonadRandom m => m Int
nonNegIntR = toInt <$> getRandomBytes 4
  where
    toInt :: ByteString -> Int
    toInt bs =
        let [a, b, c, d] = map fromIntegral $ unpack bs
        in  a + 256 * b + 65536 * c + 16777216 * d

genNatBetween :: Natural -> Natural -> Gen Natural
genNatBetween from to = do
    i <- choose (toInteger from, toInteger to)
    return $ fromIntegral i

genNat :: Gen Natural
genNat = do
    NonNegative i <- arbitrary :: Gen (NonNegative Integer)
    return $ fromIntegral i

shrinkNat :: Natural -> [Natural]
shrinkNat = map fromIntegral . shrink . toInteger

newtype Seed = Seed {getSeed :: (Word64, Word64, Word64, Word64, Word64)}
    deriving (Show, Eq, Ord, Serialise)

instance Arbitrary Seed where

    arbitrary = do  (\w1 w2 w3 w4 w5 -> Seed (w1, w2, w3, w4, w5))
                <$> gen <*> gen <*> gen <*> gen <*> gen
      where
        gen = getLarge <$> arbitrary

    shrink = const []

withSeed :: Seed -> MonadPseudoRandom ChaChaDRG a -> a
withSeed s = fst . withDRG (drgNewTest $ getSeed s)

seedToChaCha :: Seed -> ChaChaDRG
seedToChaCha = drgNewTest . getSeed

-- | Monad transformer version of 'MonadPseudoRandom'
newtype MonadPseudoRandomT gen m a = MonadPseudoRandomT {
      unMonadPseudoRandomT :: StateT gen m a
    }
  deriving (Functor, Applicative, Monad, MonadTrans)

instance (Monad m, DRG gen) => MonadRandom (MonadPseudoRandomT gen m) where
  getRandomBytes = MonadPseudoRandomT . state . randomBytesGenerate

-- | Run the 'MonadPseudoRandomT' monad
--
-- This is the analogue of cryptonite's 'withDRG'.
withDRGT :: MonadPseudoRandomT gen m a -> gen -> m (a, gen)
withDRGT = runStateT  . unMonadPseudoRandomT

generateElement :: MonadRandom m => [a] -> m (Maybe a)
generateElement [] = return Nothing
generateElement xs = do
    i <- fromIntegral <$> generateBetween 0 (genericLength xs - 1)
    return $ Just $ xs !! i
