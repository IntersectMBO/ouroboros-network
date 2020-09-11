{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies  #-}
-- | Utilities for arguments record with defaults
--
-- Useful for when you want to define a default value of an arguments record
-- consisting of a mix of arguments with/without defaults.
--
-- The following code example explains it best:
--
-- > data Args f = Args {
-- >       hasNoDefault :: HKD f Int
-- >     , hasDefault   :: Bool
-- >     }
-- >
-- > defaultArgs :: Args Defaults
-- > defaultArgs = Args {
-- >       hasNoDefault = NoDefault
-- >     , hasDefault   = False
-- >     }
-- >
-- > theArgs :: Args Identity
-- > theArgs = defaultArgs {
-- >       hasNoDefault = 0
-- >     }
-- >
-- > useArgs :: Args Identity -> (Int, Bool)
-- > useArgs (Args a b) = (a, b)
--
-- Leaving out the 'hasNoDefault' field from 'theArgs' will result in a type
-- error.
module Ouroboros.Consensus.Util.Args (
    Defaults (..)
  , HKD
  , MapHKD (..)
    -- * Re-exported for convenience
  , Identity (..)
  ) where

import           Data.Functor.Identity (Identity (..))

data Defaults t = NoDefault
  deriving (Functor)

type family HKD f a where
  HKD Identity a = a
  HKD f        a = f a

class MapHKD f where
  mapHKD :: proxy (f b) -> (a -> b) -> HKD f a -> HKD f b

instance MapHKD Identity where
  mapHKD _ = id

instance MapHKD Defaults where
  mapHKD _ _ = const NoDefault


