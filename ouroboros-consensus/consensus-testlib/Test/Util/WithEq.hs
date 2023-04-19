{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Test.Util.WithEq (
    Id (..)
  , WithEq (..)
  ) where

import           Data.Function (on)
import           GHC.Generics (Generic)

newtype Id = Id Word
  deriving stock   (Show, Generic)
  deriving newtype (Eq, Ord, Enum, Bounded, Num)

-- | Use this type to add an `Eq` instance for types that don't have one or
-- for which one doesn't make sense, but an `Eq` instance is needed for
-- testing purposes.
--
-- E.g., `ImmutableDB.Iterator` needs an `Eq` instance in the q-s-m tests
data WithEq a = WithEq
    { getId    :: Id
    , unWithEq :: a
    }
  deriving (Show, Generic)

instance Eq (WithEq a) where
  (==) = (==) `on` getId
