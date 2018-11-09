{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ouroboros.Consensus.Util.Random
    ( Seed
    , nonNegIntR
    , MonadPseudoRandomT -- opaque
    , withDRGT
    , seedToChaCha
    )
    where

import           Control.Monad.State
import           Crypto.Random (ChaChaDRG, DRG, MonadRandom (..), drgNewTest,
                     randomBytesGenerate)
import           Data.ByteString (ByteString, unpack)
import           Data.Word (Word64)
import           Test.QuickCheck

nonNegIntR :: MonadRandom m => m Int
nonNegIntR = toInt <$> getRandomBytes 4
  where
    toInt :: ByteString -> Int
    toInt bs =
        let [a, b, c, d] = map fromIntegral $ unpack bs
        in  a + 256 * b + 65536 * c + 16777216 * d

newtype Seed = Seed {getSeed :: (Word64, Word64, Word64, Word64, Word64)}
    deriving (Show, Eq, Ord)

instance Arbitrary Seed where

    arbitrary =  (\w1 w2 w3 w4 w5 -> Seed (w1, w2, w3, w4, w5))
                <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

    shrink = const []

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
