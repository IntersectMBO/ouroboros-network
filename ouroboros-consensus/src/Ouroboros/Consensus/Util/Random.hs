{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Ouroboros.Consensus.Util.Random (
      -- * Producing values in MonadRandom
      nonNegIntR
    , generateElement
      -- * Producing values in Gen
    , genNatBetween
    , genNat
    , shrinkNat
      -- * Connecting MonadRandom to Gen
    , Seed (..)
    , withSeed
    , seedToChaCha
    , nullSeed
      -- * Adding DRNG to a monad stack
    , ChaChaT -- opaque
    , runChaChaT
      -- * Convenience re-exports
    , MonadRandom (..)
    , ChaChaDRG
    )
    where

import           Control.Monad.State
import           Crypto.Number.Generate (generateBetween)
import           Crypto.Random (ChaChaDRG, MonadPseudoRandom, MonadRandom (..),
                     drgNewTest, randomBytesGenerate, withDRG)
import           Data.ByteString (ByteString, unpack)
import           Data.List (genericLength)
import           Data.Word (Word64)
import           Numeric.Natural (Natural)
import           Test.QuickCheck

import           Control.Monad.Class.MonadSay

import           Ouroboros.Network.Serialise (Serialise)

{-------------------------------------------------------------------------------
  Producing values in MonadRandom
-------------------------------------------------------------------------------}

nonNegIntR :: MonadRandom m => m Int
nonNegIntR = toInt <$> getRandomBytes 4
  where
    toInt :: ByteString -> Int
    toInt bs =
        let [a, b, c, d] = map fromIntegral $ unpack bs
        in  a + 256 * b + 65536 * c + 16777216 * d

generateElement :: MonadRandom m => [a] -> m (Maybe a)
generateElement [] = return Nothing
generateElement xs = do
    i <- fromIntegral <$> generateBetween 0 (genericLength xs - 1)
    return $ Just $ xs !! i

{-------------------------------------------------------------------------------
  Producing values in Gen
-------------------------------------------------------------------------------}

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

{-------------------------------------------------------------------------------
  Connecting MonadRandom to Gen
-------------------------------------------------------------------------------}

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

nullSeed :: Seed
nullSeed = Seed (0,0,0,0,0)

{-------------------------------------------------------------------------------
  Adding DRNG to a monad stack
-------------------------------------------------------------------------------}

-- | Add DRNG to a monad stack
newtype ChaChaT m a = ChaChaT { unChaChaT :: StateT ChaChaDRG m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadSay)

instance Monad m => MonadRandom (ChaChaT m) where
  getRandomBytes = ChaChaT . state . randomBytesGenerate

-- | Run the 'MonadPseudoRandomT' monad
--
-- This is the analogue of cryptonite's 'withDRG'.
runChaChaT :: ChaChaT m a -> ChaChaDRG -> m (a, ChaChaDRG)
runChaChaT = runStateT  . unChaChaT
