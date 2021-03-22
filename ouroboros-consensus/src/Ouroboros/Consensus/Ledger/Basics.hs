{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

-- | Definition is 'IsLedger'
--
-- Normally this is imported from "Ouroboros.Consensus.Ledger.Abstract". We
-- pull this out to avoid circular module dependencies.
module Ouroboros.Consensus.Ledger.Basics (
    -- * GetTip
    GetTip (..)
  , getTipHash
  , getTipSlot
    -- * Definition of a ledger independent of a choice of block
  , IsLedger (..)
  , LedgerCfg
    -- * Link block to its ledger
  , LedgerConfig
  , LedgerError
  , LedgerState
  , TickedLedgerState
  ) where

import           Data.Kind (Type)
import           NoThunks.Class (NoThunks)

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Ticked

{-------------------------------------------------------------------------------
  Tip
-------------------------------------------------------------------------------}

class GetTip l where
  -- | Point of the most recently applied block
  --
  -- Should be 'genesisPoint' when no blocks have been applied yet
  getTip :: l -> Point l

type instance HeaderHash (Ticked l) = HeaderHash l

getTipHash :: GetTip l => l -> ChainHash l
getTipHash = pointHash . getTip

getTipSlot :: GetTip l => l -> WithOrigin SlotNo
getTipSlot = pointSlot . getTip

{-------------------------------------------------------------------------------
  Definition of a ledger independent of a choice of block
-------------------------------------------------------------------------------}

-- | Static environment required for the ledger
type family LedgerCfg l :: Type

class ( -- Requirements on the ledger state itself
        Show     l
      , Eq       l
      , NoThunks l
        -- Requirements on 'LedgerCfg'
      , NoThunks (LedgerCfg l)
        -- Requirements on 'LedgerErr'
      , Show     (LedgerErr l)
      , Eq       (LedgerErr l)
      , NoThunks (LedgerErr l)
        -- Get the tip
        --
        -- See comment for 'applyChainTick' about the tip of the ticked ledger.
      , GetTip l
      , GetTip (Ticked l)
      ) => IsLedger l where
  -- | Errors that can arise when updating the ledger
  --
  -- This is defined here rather than in 'ApplyBlock', since the /type/ of
  -- these errors does not depend on the type of the block.
  type family LedgerErr l :: Type

  -- | Apply "slot based" state transformations
  --
  -- When a block is applied to the ledger state, a number of things happen
  -- purely based on the slot number of that block. For example:
  --
  -- * In Byron, scheduled updates are applied, and the update system state is
  --   updated.
  -- * In Shelley, delegation state is updated (on epoch boundaries).
  --
  -- The consensus layer must be able to apply such a "chain tick" function,
  -- primarily when validating transactions in the mempool (which, conceptually,
  -- live in "some block in the future") or when extracting valid transactions
  -- from the mempool to insert into a new block to be produced.
  --
  -- This is not allowed to throw any errors. After all, if this could fail,
  -- it would mean a /previous/ block set up the ledger state in such a way
  -- that as soon as a certain slot was reached, /any/ block would be invalid.
  --
  -- PRECONDITION: The slot number must be strictly greater than the slot at
  -- the tip of the ledger (except for EBBs, obviously..).
  --
  -- NOTE: 'applyChainTick' should /not/ change the tip of the underlying ledger
  -- state, which should still refer to the most recent applied /block/. In
  -- other words, we should have
  --
  -- >    ledgerTipPoint (applyChainTick cfg slot st)
  -- > == ledgerTipPoint st
  applyChainTick :: LedgerCfg l -> SlotNo -> l -> Ticked l

{-------------------------------------------------------------------------------
  Link block to its ledger
-------------------------------------------------------------------------------}

-- | Ledger state associated with a block
data family LedgerState blk :: Type

type instance HeaderHash (LedgerState blk) = HeaderHash blk

type LedgerConfig      blk = LedgerCfg (LedgerState blk)
type LedgerError       blk = LedgerErr (LedgerState blk)
type TickedLedgerState blk = Ticked    (LedgerState blk)
