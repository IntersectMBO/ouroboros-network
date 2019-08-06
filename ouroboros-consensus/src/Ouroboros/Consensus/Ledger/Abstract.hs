{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Interface to the ledger layer
module Ouroboros.Consensus.Ledger.Abstract (
    -- * Interaction with the ledger layer
    UpdateLedger(..)
  , BlockProtocol
  , ProtocolLedgerView(..)
  ) where

import           Control.Monad.Except
import           GHC.Stack (HasCallStack)

import           Ouroboros.Network.Block (Point, SlotNo)
import           Ouroboros.Network.Point (WithOrigin)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.SlotBounded (SlotBounded)

{-------------------------------------------------------------------------------
  Interaction with the ledger layer
-------------------------------------------------------------------------------}

-- | Interaction with the ledger layer
class ( SupportedBlock blk
      , Show (LedgerState blk)
      , Show (LedgerError blk)
      , Eq   (LedgerState blk)
      ) => UpdateLedger blk where
  data family LedgerState blk :: *
  type family LedgerError blk :: *

  -- | Static environment required for the ledger
  data family LedgerConfig blk :: *

  -- | Extract the ledger environment from the node config
  ledgerConfigView :: NodeConfig (BlockProtocol blk)
                   -> LedgerConfig blk

  -- | Apply state transformations that might occur when encountering a new
  --   block, but happen before full header and body processing. In the Byron
  --   era this is used to perform epoch transitions on receipt of the first
  --   block in the new epoch.
  applyChainTick :: LedgerConfig blk
                 -> SlotNo
                 -> LedgerState blk
                 -> Except (LedgerError blk) (LedgerState blk)

  -- | Apply a block to the ledger state.
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

-- | Link protocol to ledger
class UpdateLedger blk => ProtocolLedgerView blk where
  protocolLedgerView :: NodeConfig (BlockProtocol blk)
                     -> LedgerState blk
                     -> LedgerView (BlockProtocol blk)

  -- | Get a ledger view for a specific slot
  --
  -- Suppose @k = 4@, i.e., we can roll back 4 blocks
  --
  -- >             /-----------\
  -- >             |           ^
  -- >             v           |
  -- >     --*--*--*--*--*--*--*--
  -- >          |  A           B
  -- >          |
  -- >          \- A'
  --
  -- In other words, this means that we can roll back from point B to point A,
  -- and then roll forward to any block on any fork from A. Note that we can
  -- /not/ roll back to any siblings of A (such as A'), as that would require
  -- us to roll back at least @k + 1@ blocks, which we can't (by definition).
  --
  -- Given a ledger state at point B, we should be able to verify any of the
  -- headers (corresponding to the blocks) at point A or any of its successors
  -- on any fork, up to some maximum distance from A. This distance can be
  -- determined by the ledger, though must be at least @k@: we must be able to
  -- validate any of these past headers, since otherwise we would not be able to
  -- switch to a fork. It is not essential that the maximum distance extends
  -- into the future (@> k@), though it is helpful: it means that in the chain
  -- sync client we can download and validate headers even if they don't fit
  -- directly onto the tip of our chain.
  --
  -- The anachronistic ledger state at point B is precisely the ledger state
  -- that can be used to validate this set of headers. The bounds (in terms of
  -- slots) are a hint about its valid range: how far into the past can we look
  -- (at least @k@) and how far into the future (depending on the maximum
  -- distance supported by the ledger). It is however important to realize that
  -- this is not a full specification: after all, blocks @A@ and @A'@ have the
  -- same slot number, but @A@ can be validated using the anachronistic ledger
  -- view at @B@ whereas @A'@ can not.
  --
  -- Invariant: when calling this function with slot @s@ yields a
  -- 'SlotBounded' @sb@, then @'atSlot' sb@ yields a 'Just'.
  anachronisticProtocolLedgerView
    :: NodeConfig (BlockProtocol blk)
    -> LedgerState blk
    -> WithOrigin SlotNo -- ^ Slot for which you would like a ledger view
    -> Maybe (SlotBounded (LedgerView (BlockProtocol blk)))
