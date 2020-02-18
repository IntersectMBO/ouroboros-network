{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}

module Ouroboros.Consensus.Block.SupportsProtocol (
    SupportedBlock(..)
  ) where

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Protocol.Abstract

{-------------------------------------------------------------------------------
  Supported blocks
-------------------------------------------------------------------------------}

-- | Evidence that a block supports its protocol
--
-- TODO: Rename to 'BlockSupportsProtocol'.
class ( GetHeader blk
      , HasHeader blk
      , HasHeader (Header blk)
      , OuroborosTag (BlockProtocol blk)
      , NoUnexpectedThunks (Header blk)
      ) => SupportedBlock blk where
  validateView :: NodeConfig (BlockProtocol blk)
               -> Header blk -> ValidateView (BlockProtocol blk)

  selectView   :: NodeConfig (BlockProtocol blk)
               -> Header blk -> SelectView   (BlockProtocol blk)

  -- Default chain selection just looks at longest chains
  default selectView :: SelectView (BlockProtocol blk) ~ BlockNo
                     => NodeConfig (BlockProtocol blk)
                     -> Header blk -> SelectView (BlockProtocol blk)
  selectView _ = blockNo
