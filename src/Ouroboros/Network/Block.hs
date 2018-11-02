{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

-- | Abstract view over blocks
--
-- The network layer does not make any concrete assumptions about what blocks
-- look like.
module Ouroboros.Network.Block (
    Slot(..)
  , BlockNo(..)
  , HasHeader(..)
  , Hash(..)
  , castHash
  ) where

import           Data.Hashable
import           GHC.Generics (Generic)

import           Ouroboros.Network.Serialise

-- | The Ouroboros time slot index for a block.
newtype Slot = Slot { getSlot :: Word }
  deriving (Show, Eq, Ord, Hashable, Enum)

-- | The 0-based index of the block in the blockchain
newtype BlockNo = BlockNo Word
  deriving (Show, Eq, Ord, Hashable, Enum)

-- | Abstract over the shape of blocks (or indeed just block headers)
class ( Eq        (HeaderHash b)
      , Ord       (HeaderHash b)
      , Show      (HeaderHash b)
      , Serialise (HeaderHash b)
      , Hashable  (HeaderHash b)
      ) => HasHeader b where
    -- TODO: I /think/ we should be able to make this injective, but I'd have
    -- to check after the redesign of the block abstraction (which will live
    -- in the consensus layer), to make sure injectivity is compatible with that
    -- design (and compatible with the concrete instantiation used in the
    -- network layer tests).
    type HeaderHash b :: *

    blockHash      :: b -> HeaderHash b
    blockPrevHash  :: b -> Hash b
    blockSlot      :: b -> Slot
    blockNo        :: b -> BlockNo

    blockInvariant :: b -> Bool

data Hash b = GenesisHash | BlockHash (HeaderHash b)
  deriving (Generic)

deriving instance HasHeader block => Eq       (Hash block)
deriving instance HasHeader block => Ord      (Hash block)
deriving instance HasHeader block => Show     (Hash block)

instance HasHeader block => Hashable (Hash block)
  -- use generic instance

castHash :: HeaderHash b ~ HeaderHash b' => Hash b -> Hash b'
castHash GenesisHash   = GenesisHash
castHash (BlockHash b) = BlockHash b

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

instance Serialise Slot where
  encode (Slot s) = encodeWord s
  decode = Slot <$> decodeWord

instance HasHeader b => Serialise (Hash b) where
  -- use the Generic instance
