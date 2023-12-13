{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Ouroboros.Network.SizeInBytes (SizeInBytes (..)) where

import           Codec.Serialise (Serialise)
import           Control.DeepSeq (NFData (..))
import           Data.Word (Word32)

import           NoThunks.Class (NoThunks (..))

newtype SizeInBytes = SizeInBytes { getSizeInBytes :: Word32 }
  deriving (Show, Eq, Ord)
  deriving Enum     via Word32
  deriving Num      via Word32
  deriving Real     via Word32
  deriving Serialise     via Word32
  deriving Integral via Word32
  deriving NoThunks via Word32
  deriving newtype NFData
