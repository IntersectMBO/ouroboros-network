{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

-- | Abstract view over blocks
--
-- The network layer does not make any concrete assumptions about what blocks
-- look like.
module Block (
    Slot(..)
  , BlockNo(..)
  , HasHeader(..)
  ) where

import           Data.Hashable

import           Serialise

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
      ) => HasHeader b where
    -- TODO: I /think/ we should be able to make this injective, but I'd have
    -- to check after the redesign of the block abstraction (which will live
    -- in the consensus layer), to make sure injectivity is compatible with that
    -- design (and compatible with the concrete instantiation used in the
    -- network layer tests).
    type HeaderHash b :: *

    blockHash      :: b -> HeaderHash b
    blockPrevHash  :: b -> HeaderHash b
    blockSlot      :: b -> Slot
    blockNo        :: b -> BlockNo

    blockInvariant :: b -> Bool

    -- TODO: This really shouldn't exist at all. There _is_ no genesis block,
    -- and it should have no hash.
    genesisHash :: proxy b -> HeaderHash b

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

instance Serialise Slot where
  encode (Slot s) = encodeWord s
  decode = Slot <$> decodeWord
