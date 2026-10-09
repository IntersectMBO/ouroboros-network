module Ouroboros.Network.Hashable
  ( Hashable
  , Salt
  , mkSalt
  , mkSaltIO
  , mkUnsafeSalt
  , hashWithSalt
  ) where

import Data.Bifunctor (first)
import Data.Hashable (Hashable)
import Data.Hashable qualified as Hashable
import System.Random (RandomGen, random, randomIO)

newtype Salt = Salt Int
  deriving Show

-- | `Salt` can be constructed from a random generator.
--
mkSalt :: RandomGen g => g -> (Salt, g)
mkSalt = first Salt . random

-- | Generate a new `Salt` in `IO` monad.
--
mkSaltIO :: IO Salt
mkSaltIO = Salt <$> randomIO

-- | Unsafe constructor, should only be used in tests.
--
mkUnsafeSalt :: Int -> Salt
mkUnsafeSalt = Salt

hashWithSalt :: Hashable a => Salt -> a -> Int
hashWithSalt (Salt salt) a = Hashable.hashWithSalt salt a
