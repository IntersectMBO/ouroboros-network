{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ouroboros.Storage.Common (
    -- * Epochs
    EpochNo(..)
  , EpochSize(..)
    -- * File formats
  , SlotOffset
    -- * Indexing
  , Tip(..)
  , tipIsGenesis
  ) where

import           Codec.Serialise
import           Data.Word
import           GHC.Generics

{-------------------------------------------------------------------------------
  Epochs
-------------------------------------------------------------------------------}

-- | An epoch, i.e. the number of the epoch.
newtype EpochNo = EpochNo { unEpochNo :: Word64 }
  deriving (Eq, Ord, Enum, Num, Show, Generic)

newtype EpochSize = EpochSize { unEpochSize :: Word64 }
  deriving (Eq, Ord, Enum, Num, Show, Generic, Real, Integral)

{-------------------------------------------------------------------------------
  File formats
-------------------------------------------------------------------------------}

-- | The offset of a slot in an index file.
type SlotOffset = Word64

{-------------------------------------------------------------------------------
  Indexing
-------------------------------------------------------------------------------}

-- | Tip of the chain
data Tip r = Tip r | TipGen
  deriving (Show, Eq, Generic)

tipIsGenesis :: Tip r -> Bool
tipIsGenesis TipGen  = True
tipIsGenesis (Tip _) = False

{-------------------------------------------------------------------------------
  Serialization
-------------------------------------------------------------------------------}

instance (Serialise r) => Serialise (Tip r)
  -- TODO: Don't use generic instance
