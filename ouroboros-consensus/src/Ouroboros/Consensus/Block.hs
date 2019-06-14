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
  ) where

import           Data.FingerTree (Measured (..))

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Protocol.Abstract

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
headerPoint = Point . blockPoint

{-------------------------------------------------------------------------------
  Supported blocks
-------------------------------------------------------------------------------}

class ( GetHeader blk
      , HasHeader blk
      , HasHeader (Header blk)
      , OuroborosTag (BlockProtocol blk)
      , SupportedHeader (BlockProtocol blk) (Header blk)
      ) => SupportedBlock blk
