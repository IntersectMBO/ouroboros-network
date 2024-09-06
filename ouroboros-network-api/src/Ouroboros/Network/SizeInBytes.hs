{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Ouroboros.Network.SizeInBytes (
    SizeInBytes (..)
  , WithBytes (..)) where

import Control.DeepSeq (NFData (..))
import Data.ByteString.Short (ShortByteString)
import Data.Monoid (Sum (..))
import Data.Word (Word32)
import GHC.Generics

import Data.Measure qualified as Measure
import NoThunks.Class (NoThunks (..))
import Quiet (Quiet (..))

data WithBytes a = WithBytes { wbValue    :: !a,
                               unannotate :: !ShortByteString }
  deriving (Eq, Show)

instance NFData a => NFData (WithBytes a) where
  rnf (WithBytes a b) = rnf a `seq` rnf b

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
  deriving Measure.Measure        via Word32
  deriving Measure.BoundedMeasure via Word32
