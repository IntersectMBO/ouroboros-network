{-# LANGUAGE DeriveAnyClass          #-}
{-# LANGUAGE DeriveGeneric           #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | The consensus layer's abstract view of blocks
module Ouroboros.Consensus.Block (
    -- * Link block to its header
    BlockProtocol
  , GetHeader(..)
    -- * Wrappers around 'HasHeader' and co
  , headerHash
  , headerPrevHash
  , headerPoint
    -- * Supported blocks
  , SupportedBlock
    -- * EBBs
  , IsEBB (..)
  , toIsEBB
  , fromIsEBB
  ) where

import           Codec.Serialise (Serialise)
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)
import           Data.FingerTree.Strict (Measured (..))

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.Condense

{-------------------------------------------------------------------------------
  Link block to its header
-------------------------------------------------------------------------------}

-- | Map block to consensus protocol
--
-- We do not expect instances for headers.
type family BlockProtocol blk :: *

class GetHeader blk where
  data family Header blk :: *
  getHeader :: blk -> Header blk

type instance BlockProtocol (Header blk) = BlockProtocol blk

{-------------------------------------------------------------------------------
  Some automatic instances for 'Header'

  Unfortunately we cannot give a 'HasHeader' instance; if we mapped from a
  header to a block instead we could do

  > instance HasHeader hdr => HasHeader (Block hdr) where
  >  ..

  but we can't do that when we do things this way around.
-------------------------------------------------------------------------------}

type instance HeaderHash (Header blk) = HeaderHash blk

instance HasHeader blk => StandardHash (Header blk)

instance HasHeader (Header blk) => Measured BlockMeasure (Header blk) where
  measure = blockMeasure

{-------------------------------------------------------------------------------
  Convenience wrappers around 'HasHeader' that avoids unnecessary casts
-------------------------------------------------------------------------------}

headerHash :: HasHeader (Header blk) => Header blk -> HeaderHash blk
headerHash = blockHash

headerPrevHash :: HasHeader (Header blk) => Header blk -> ChainHash blk
headerPrevHash = castHash . blockPrevHash

headerPoint :: HasHeader (Header blk) => Header blk -> Point blk
headerPoint = castPoint . blockPoint

{-------------------------------------------------------------------------------
  Supported blocks
-------------------------------------------------------------------------------}

class ( GetHeader blk
      , HasHeader blk
      , HasHeader (Header blk)
      , OuroborosTag (BlockProtocol blk)
      , CanValidate  (BlockProtocol blk) (Header blk)
      , CanSelect    (BlockProtocol blk) (Header blk)
      , NoUnexpectedThunks (Header blk)
      ) => SupportedBlock blk

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
  deriving (Eq, Show, Generic, NoUnexpectedThunks, Serialise)

instance Condense IsEBB where
  condense = show

toIsEBB :: Bool -> IsEBB
toIsEBB b = if b then IsEBB else IsNotEBB

fromIsEBB :: IsEBB -> Bool
fromIsEBB IsEBB    = True
fromIsEBB IsNotEBB = False
