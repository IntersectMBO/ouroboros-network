{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Interface to the ledger layer
module Ouroboros.Consensus.Ledger.Abstract (
    -- * Interaction with the ledger layer
    UpdateLedger(..)
  , TickedLedgerState(..)
  , ledgerTipHash
  , ledgerTipSlot
  , BlockProtocol
  , QueryLedger(..)
  , ShowQuery(..)
  ) where

import           Control.Monad.Except
import           Data.Type.Equality ((:~:))
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Network.Block (ChainHash, HasHeader, Point, SlotNo,
                     pointHash, pointSlot)
import           Ouroboros.Network.Point (WithOrigin)
import           Ouroboros.Network.Protocol.LocalStateQuery.Type
                     (ShowQuery (..))

import           Ouroboros.Consensus.Block.Abstract

{-------------------------------------------------------------------------------
  Interaction with the ledger layer
-------------------------------------------------------------------------------}

-- | Interaction with the ledger layer
class ( HasHeader blk
      , HasHeader (Header blk)
      , Show (LedgerState blk)
      , Show (LedgerError blk)
      , Eq   (LedgerState blk)
      , Eq   (LedgerError blk)
      , NoUnexpectedThunks (LedgerState  blk)
      , NoUnexpectedThunks (LedgerError  blk)
      , NoUnexpectedThunks (LedgerConfig blk)
      ) => UpdateLedger blk where
  data family LedgerState blk :: *
  type family LedgerError blk :: *

  -- | Static environment required for the ledger
  data family LedgerConfig blk :: *

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
  -- the tip of the ledger.
  applyChainTick :: LedgerConfig blk
                 -> SlotNo
                 -> LedgerState blk
                 -> TickedLedgerState blk

  -- | Apply a block to the ledger state.
  --
  -- This should apply the /entire/ block (i.e., including 'applyChainTick').
  applyLedgerBlock :: LedgerConfig blk
                   -> blk
                   -> LedgerState blk
                   -> Except (LedgerError blk) (LedgerState blk)

  -- | Re-apply a block to the very same ledger state it was applied in before.
  --
  -- Since a block can only be applied to a single, specific, ledger state,
  -- if we apply a previously applied block again it will be applied in the
  -- very same ledger state, and therefore can't possibly fail.
  --
  -- It is worth noting that since we already know that the block is valid in
  -- the provided ledger state, the ledger layer should not perform /any/
  -- validation checks.
  reapplyLedgerBlock :: HasCallStack
                     => LedgerConfig blk
                     -> blk
                     -> LedgerState blk
                     -> LedgerState blk

  -- | Point of the most recently applied block
  --
  -- Should be 'genesisPoint' when no blocks have been applied yet
  ledgerTipPoint :: LedgerState blk -> Point blk

ledgerTipHash :: UpdateLedger blk => LedgerState blk -> ChainHash blk
ledgerTipHash = pointHash . ledgerTipPoint

ledgerTipSlot :: UpdateLedger blk => LedgerState blk -> WithOrigin SlotNo
ledgerTipSlot = pointSlot . ledgerTipPoint

-- | Ledger state with the chain tick function already applied
--
-- 'applyChainTick' is intended to mark the passage of time, without changing
-- the tip of the underlying ledger (i.e., no blocks have been applied).
data TickedLedgerState blk = TickedLedgerState {
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
    , tickedLedgerState :: !(LedgerState blk)
    }
  deriving (Generic)

deriving instance NoUnexpectedThunks       (LedgerState blk)
               => NoUnexpectedThunks (TickedLedgerState blk)

-- | Query the ledger state.
--
-- Used by the LocalStateQuery protocol to allow clients to query the ledger
-- state.
class (UpdateLedger blk, ShowQuery (Query blk)) => QueryLedger blk where

  -- | Different queries supported by the ledger, indexed by the result type.
  data family Query  blk :: * -> *

  -- | Answer the given query about the ledger state.
  answerQuery :: LedgerConfig blk -> Query blk result -> LedgerState blk -> result

  -- | Generalisation of value-level equality of two queries.
  eqQuery :: Query blk result1 -> Query blk result2
          -> Maybe (result1 :~: result2)
