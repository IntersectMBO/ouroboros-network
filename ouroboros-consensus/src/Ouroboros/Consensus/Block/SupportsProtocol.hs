{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}

module Ouroboros.Consensus.Block.SupportsProtocol (
    BlockSupportsProtocol (..)
  ) where

import           NoThunks.Class (NoThunks)

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Protocol.Abstract

{-------------------------------------------------------------------------------
  Supported blocks
-------------------------------------------------------------------------------}

-- | Evidence that a block supports its protocol
class ( GetHeader blk
      , GetPrevHash blk
      , ConsensusProtocol (BlockProtocol blk)
      , NoThunks (Header blk)
      , NoThunks (BlockConfig blk)
      , NoThunks (CodecConfig blk)
      , NoThunks (StorageConfig blk)
      ) => BlockSupportsProtocol blk where
  validateView :: BlockConfig blk
               -> Header blk
               -> ValidateView (BlockProtocol blk)

  selectView   :: BlockConfig blk
               -> Header blk -> SelectView   (BlockProtocol blk)

  -- Default chain selection just looks at longest chains
  default selectView :: SelectView (BlockProtocol blk) ~ BlockNo
                     => BlockConfig blk
                     -> Header blk -> SelectView (BlockProtocol blk)
  selectView _ = blockNo
