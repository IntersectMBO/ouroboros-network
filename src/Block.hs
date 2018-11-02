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
  , BlockSigner(..)
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

-- | An identifier for someone signing a block.
--
-- We model this as if there were an enumerated set of valid block signers
-- (which for Ouroboros BFT is actually the case), and omit the cryptography
-- and model things as if the signatures were valid.
--
-- TODO: This should go completely; the network layer should not have to
-- think about signatures at all.
newtype BlockSigner = BlockSigner Word
  deriving (Show, Eq, Ord, Hashable)

-- | Abstract over the shape of blocks (or indeed just block headers)
--
-- TODO: does the network layer really need to be aware of body hashes? I think
-- that can go.
class ( Eq        (HeaderHash b)
      , Ord       (HeaderHash b)
      , Show      (HeaderHash b)
      , Serialise (HeaderHash b)
      , Eq        (BodyHash   b)
      , Ord       (BodyHash   b)
      , Show      (BodyHash   b)
      , Serialise (BodyHash   b)
      ) => HasHeader b where
    -- TODO: I /think/ we should be able to make these injective, but I'd have
    -- to check after the redesign of the block abstraction (which will live
    -- in the consensus layer), to make sure injectivity is compatible with that
    -- design (and compatible with the concrete instantiation used in the
    -- network layer tests).
    type HeaderHash b :: *
    type BodyHash   b :: *

    blockHash      :: b -> HeaderHash b
    blockPrevHash  :: b -> HeaderHash b
    blockSlot      :: b -> Slot
    blockNo        :: b -> BlockNo
    blockSigner    :: b -> BlockSigner
    blockBodyHash  :: b -> BodyHash b

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
