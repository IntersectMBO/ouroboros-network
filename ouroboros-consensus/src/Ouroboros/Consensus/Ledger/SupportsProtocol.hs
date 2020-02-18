module Ouroboros.Consensus.Ledger.SupportsProtocol (
    ProtocolLedgerView(..)
  , AnachronyFailure(..)
  ) where

import           Ouroboros.Network.Block (SlotNo)
import           Ouroboros.Network.Point (WithOrigin)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.Abstract

-- | Link protocol to ledger
--
-- TODO: Rename to 'LedgerSupportsProtocol'
class ( SupportedBlock   blk
      , UpdateLedger     blk
      , ValidateEnvelope blk
      ) => ProtocolLedgerView blk where
  -- | Extract the ledger environment from the node config
  ledgerConfigView :: NodeConfig (BlockProtocol blk)
                   -> LedgerConfig blk

  -- | Extract ledger view from the ledger state
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
  -- that can be used to validate this set of headers.
  --
  -- Invariant: when calling this function with slot @s@ yields a
  -- 'SlotBounded' @sb@, then @'atSlot' sb@ yields a 'Just'.
  anachronisticProtocolLedgerView
    :: NodeConfig (BlockProtocol blk)
    -> LedgerState blk
    -> WithOrigin SlotNo -- ^ Slot for which you would like a ledger view
    -> Either AnachronyFailure (LedgerView (BlockProtocol blk))

-- | See 'anachronisticProtocolLedgerView'.
data AnachronyFailure
  = TooFarAhead
  | TooFarBehind
  deriving (Eq,Show)
