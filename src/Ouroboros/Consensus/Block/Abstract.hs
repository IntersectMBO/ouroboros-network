{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}

-- | Abstract definition of a block
module Ouroboros.Consensus.Block.Abstract (
    BlockTag(..)
  ) where

{-------------------------------------------------------------------------------
  Abstract over blocks
-------------------------------------------------------------------------------}

-- | The (open) universe of blocks
class BlockTag (b :: *) where
  data family Block       b :: *
  data family BlockHeader b :: *
  data family BlockBody   b :: *
  type family LedgerState b :: *

  -- TODO: Not entirely sure we need these
  blockHeader :: Block b -> BlockHeader b
  blockBody   :: Block b -> BlockBody   b

  -- Apply a block to the ledger state
  --
  -- This is the key part of this type class.
  --
  -- TODO: We need to support rollback, so this probably won't be a pure
  -- function but rather something that lives in a monad with some actions
  -- that we can compute a "running diff" so that we can go back in time.
  applyBlock  :: Block b -> LedgerState b -> LedgerState b
