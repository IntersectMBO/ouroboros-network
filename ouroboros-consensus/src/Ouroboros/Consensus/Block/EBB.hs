{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

-- | Generic infrastructure for working with EBBs
module Ouroboros.Consensus.Block.EBB (
    IsEBB (..)
  , fromIsEBB
  , toIsEBB
  ) where

import           Codec.Serialise (Serialise (..))
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Ouroboros.Consensus.Util.Condense

{-------------------------------------------------------------------------------
  EBBs
-------------------------------------------------------------------------------}

-- | Whether a block is an Epoch Boundary Block (EBB)
--
-- See "Ouroboros.Storage.ImmutableDB.API" for a discussion of EBBs. Key
-- idiosyncracies:
--
--  * An EBB carries no unique information.
--
--  * An EBB has the same 'BlockNo' as its predecessor.
--
--  * EBBs are vestigial. As of Shelley, nodes no longer forge EBBs: they are
--    only a legacy/backwards-compatibility concern.
data IsEBB
  = IsEBB
  | IsNotEBB
  deriving (Eq, Show, Generic, NoThunks)

instance Serialise IsEBB where
  encode = encode . fromIsEBB
  decode = toIsEBB <$> decode

instance Condense IsEBB where
  condense = show

toIsEBB :: Bool -> IsEBB
toIsEBB b = if b then IsEBB else IsNotEBB

fromIsEBB :: IsEBB -> Bool
fromIsEBB IsEBB    = True
fromIsEBB IsNotEBB = False
