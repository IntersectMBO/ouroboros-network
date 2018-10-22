{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ouroboros (
    -- * Typed used across all protocols
    Slot(..)
  , NodeId(..)
    -- * Generalize over the Ouroboros protocols
  , OuroborosProtocol(..)
  ) where

import           Data.Hashable
import           GHC.Generics

{-------------------------------------------------------------------------------
  Types used across all protocols
-------------------------------------------------------------------------------}

-- | The Ouroboros time slot index for a block.
newtype Slot = Slot { getSlot :: Word }
  deriving (Show, Eq, Ord, Hashable, Enum)

data NodeId = CoreId Int
            | RelayId Int
  deriving (Eq, Ord, Show, Generic)

instance Hashable NodeId -- let generic instance do the job

{-------------------------------------------------------------------------------
  Generalize over the various Ouroboros protocols
-------------------------------------------------------------------------------}

data OuroborosProtocol =
    OuroborosBFT
  | OuroborosPraos
