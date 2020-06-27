{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE TypeFamilies       #-}

-- | Definition is 'IsLedger'
--
-- Normally this is imported from "Ouroboros.Consensus.Ledger.Abstract". We
-- pull this out to avoid circular module dependencies.
module Ouroboros.Consensus.Ledger.Basics (
    -- * Definition of a ledger independent of a choice of block
    LedgerCfg
  , IsLedger(..)
    -- * Link block to its ledger
  , LedgerState
  , LedgerConfig
  , LedgerError
  , TickedLedgerState
    -- * Ticked ledger state
  , Ticked(..)
  ) where

import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Consensus.Block.Abstract

{-------------------------------------------------------------------------------
  Definition of a ledger independent of a choice of block
-------------------------------------------------------------------------------}

-- | Static environment required for the ledger
type family LedgerCfg l :: *

class ( -- Requirements on the ledger state itself
        Show               l
      , Eq                 l
      , NoUnexpectedThunks l
        -- Requirements on 'LedgerCfg'
      , NoUnexpectedThunks (LedgerCfg l)
        -- Requirements on 'LedgerErr'
      , Show               (LedgerErr l)
      , Eq                 (LedgerErr l)
      , NoUnexpectedThunks (LedgerErr l)
      ) => IsLedger l where
  -- | Errors that can arise when updating the ledger
  --
  -- This is defined here rather than in 'ApplyBlock', since the /type/ of
  -- these errors does not depend on the type of the block.
  type family LedgerErr l :: *

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
  applyChainTick :: LedgerCfg l -> SlotNo -> l -> Ticked l

{-------------------------------------------------------------------------------
  Link block to its ledger
-------------------------------------------------------------------------------}

-- | Ledger state associated with a block
data family LedgerState blk :: *

type LedgerConfig      blk = LedgerCfg (LedgerState blk)
type LedgerError       blk = LedgerErr (LedgerState blk)
type TickedLedgerState blk = Ticked    (LedgerState blk)

{-------------------------------------------------------------------------------
  Ticked ledger state
-------------------------------------------------------------------------------}

-- | Mark a ledger state or ledger view as " ticked "
--
-- Ticking refers to the passage of time (the ticking of the clock). When a
-- ledger state or ledger view is marked as ticked, it means  that time-related
-- changes have been applied to ledger state, or the right view has been
-- forecast for that slot.
--
-- Some examples of time related changes:
--
-- * Scheduled delegations might have been applied in Byron
-- * New leader schedule computed for Shelley
-- * Transition from Byron to Shelley activated in the hard fork combinator.
--
-- When applying a block, the ledger must first have been advanced to the slot
-- of the block.
data Ticked l = Ticked {
      -- | The slot number supplied to 'applyChainTick'
      tickedSlotNo      :: !SlotNo

      -- | The underlying ledger state
      --
      -- NOTE: 'applyChainTick' should /not/ change the tip of the underlying
      -- ledger state, which should still refer to the most recent applied
      -- /block/. In other words, we should have
      --
      -- >    ledgerTipPoint (tickedLedgerState (applyChainTick cfg slot st)
      -- > == ledgerTipPoint st
    , tickedLedgerState :: !l
    }
  deriving stock    (Generic, Functor)
  deriving anyclass (NoUnexpectedThunks)
