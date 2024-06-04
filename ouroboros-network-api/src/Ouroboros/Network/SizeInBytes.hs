{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric              #-}

module Ouroboros.Network.SizeInBytes (SizeInBytes (..)) where

import Control.DeepSeq (NFData (..))
import Data.Monoid (Sum (..))
import Data.Word (Word32)
import GHC.Generics

import Quiet (Quiet (..))
import NoThunks.Class (NoThunks (..))

newtype SizeInBytes = SizeInBytes { getSizeInBytes :: Word32 }
  deriving (Eq, Ord)
  deriving Show      via Quiet SizeInBytes
  deriving Enum      via Word32
  deriving Num       via Word32
  deriving Real      via Word32
  deriving Integral  via Word32
  deriving NoThunks  via Word32
  deriving Semigroup via Sum Word32
  deriving Monoid    via Sum Word32
  deriving Generic
  deriving newtype NFData
