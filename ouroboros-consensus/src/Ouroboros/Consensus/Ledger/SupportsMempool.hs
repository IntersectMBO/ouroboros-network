{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
module Ouroboros.Consensus.Ledger.SupportsMempool (
    ApplyTxErr
  , GenTx
  , GenTxId
  , HasTxId (..)
  , HasTxs (..)
  , LedgerSupportsMempool (..)
  , TxId
  , Validated
  , WhetherToIntervene (..)
  ) where

import           Control.Monad.Except
import           Data.Kind (Type)
import           Data.Word (Word32)
import           GHC.Stack (HasCallStack)

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ticked
import           Ouroboros.Consensus.Util.IOLike

-- | Generalized transaction
--
-- The mempool (and, accordingly, blocks) consist of "generalized
-- transactions"; this could be "proper" transactions (transferring funds) but
-- also other kinds of things such as update proposals, delegations, etc.
data family GenTx blk :: Type

-- | Updating the ledger with a single transaction may result in a different
-- error type as when updating it with a block
type family ApplyTxErr blk :: Type

-- | A flag indicating whether the mempool should reject a valid-but-problematic
-- transaction, in order to to protect its author from penalties etc
--
-- The primary example is that, as of the Alonzo ledger, a valid transaction can
-- carry an invalid script. If a remote peer sends us such a transaction (over a
-- Node-to-Node protocol), we include it in a block so that the ledger will
-- penalize them them for the invalid script: they wasted our resources by
-- forcing us to run the script to determine it's invalid. But if our local
-- wallet -- which we trust by assumption -- sends us such a transaction (over a
-- Node-to-Client protocol), we would be a good neighbor by rejecting that
-- transaction: they must have made some sort of mistake, and we don't want the
-- ledger to penalize them.
data WhetherToIntervene
  = DoNotIntervene
    -- ^ We do not trust remote peers, so if a problematic-yet-valid transaction
    -- arrives over NTN, we accept it; it will end up in a block and the ledger
    -- will penalize them for it.
  | Intervene
    -- ^ We trust local clients, so if a problematic-yet-valid transaction
    -- arrives over NTC, we reject it in order to avoid the ledger penalizing
    -- them for it.

class ( UpdateLedger blk
      , NoThunks (GenTx blk)
      , NoThunks (Validated (GenTx blk))
      , NoThunks (Ticked (LedgerState blk))
      , Show (GenTx blk)
      , Show (Validated (GenTx blk))
      , Show (ApplyTxErr blk)
      ) => LedgerSupportsMempool blk where

  -- | Check whether the internal invariants of the transaction hold.
  txInvariant :: GenTx blk -> Bool
  txInvariant = const True

  -- | Apply an unvalidated transaction
  applyTx :: LedgerConfig blk
          -> WhetherToIntervene
          -> SlotNo -- ^ Slot number of the block containing the tx
          -> GenTx blk
          -> TickedLedgerState blk
          -> Except (ApplyTxErr blk) (TickedLedgerState blk, Validated (GenTx blk))

  -- | Apply a previously validated transaction to a potentially different
  -- ledger state
  --
  -- When we re-apply a transaction to a potentially different ledger state
  -- expensive checks such as cryptographic hashes can be skipped, but other
  -- checks (such as checking for double spending) must still be done.
  reapplyTx :: HasCallStack
            => LedgerConfig blk
            -> SlotNo -- ^ Slot number of the block containing the tx
            -> Validated (GenTx blk)
            -> TickedLedgerState blk
            -> Except (ApplyTxErr blk) (TickedLedgerState blk)

  -- | The maximum number of bytes worth of transactions that can be put into
  -- a block according to the currently adopted protocol parameters of the
  -- ledger state.
  --
  -- This is (conservatively) computed by subtracting the header size and any
  -- other fixed overheads from the maximum block size.
  maxTxCapacity :: TickedLedgerState blk -> Word32

  -- | Return the post-serialisation size in bytes of a 'GenTx' /when it is
  -- embedded in a block/. This size might differ from the size of the
  -- serialisation used to send and receive the transaction across the
  -- network.
  --
  -- This size is used to compute how many transaction we can put in a block
  -- when forging one.
  --
  -- For example, CBOR-in-CBOR could be used when sending the transaction
  -- across the network, requiring a few extra bytes compared to the actual
  -- in-block serialisation. Another example is the transaction of the
  -- hard-fork combinator which will include an envelope indicating its era
  -- when sent across the network. However, when embedded in the respective
  -- era's block, there is no need for such envelope.
  --
  -- Can be implemented by serialising the 'GenTx', but, ideally, this is
  -- implement more efficiently. E.g., by returning the length of the
  -- annotation.
  txInBlockSize :: GenTx blk -> Word32

  -- | Discard the evidence that transaction has been previously validated
  txForgetValidated :: Validated (GenTx blk) -> GenTx blk

-- | A generalized transaction, 'GenTx', identifier.
data family TxId tx :: Type

-- | Transactions with an identifier
--
-- The mempool will use these to locate transactions, so two different
-- transactions should have different identifiers.
class ( Show     (TxId tx)
      , Ord      (TxId tx)
      , NoThunks (TxId tx)
      ) => HasTxId tx where

  -- | Return the 'TxId' of a 'GenTx'.
  --
  -- NOTE: a 'TxId' must be unique up to ledger rules, i.e., two 'GenTx's with
  -- the same 'TxId' must be the same transaction /according to the ledger/.
  -- However, we do not assume that a 'TxId' uniquely determines a 'GenTx':
  -- two 'GenTx's with the same 'TxId' can differ in, e.g., witnesses.
  --
  -- Should be cheap as this will be called often.
  txId :: tx -> TxId tx

-- | Shorthand: ID of a generalized transaction
type GenTxId blk = TxId (GenTx blk)

-- | Collect all transactions from a block
--
-- This is used for tooling only. We don't require it as part of RunNode
-- (and cannot, because we cannot give an instance for the dual ledger).
class HasTxs blk where
  -- | Return the transactions part of the given block in no particular order.
  extractTxs :: blk -> [GenTx blk]
