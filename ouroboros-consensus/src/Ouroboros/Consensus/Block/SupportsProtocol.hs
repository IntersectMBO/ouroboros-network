{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}

module Ouroboros.Consensus.Block.SupportsProtocol (
    BlockSupportsProtocol(..)
  ) where

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Protocol.Abstract

{-------------------------------------------------------------------------------
  Supported blocks
-------------------------------------------------------------------------------}

-- | Evidence that a block supports its protocol
class ( GetHeader blk
      , GetPrevHash blk
      , GetPrevHash (Header blk)
      , ConsensusProtocol (BlockProtocol blk)
      , NoUnexpectedThunks (Header blk)
      , NoUnexpectedThunks (BlockConfig blk)
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
