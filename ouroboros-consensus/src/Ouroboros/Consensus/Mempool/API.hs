{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.Mempool.API (
    -- * Mempool
    Mempool (..)
  , MempoolAddTxResult (..)
  , mempoolTxAddedToMaybe
    -- * Mempool Snapshot
  , MempoolSnapshot (..)
    -- * Mempool size and capacity
  , MempoolCapacityBytes (..)
  , MempoolCapacityBytesOverride (..)
  , MempoolSize (..)
  , computeMempoolCapacity
    -- * Tracing support
  , addLocalTxs
  , addTxs
    -- * Re-exports
  , TraceEventMempool (..)
  , TxSizeInBytes
  ) where

import qualified Data.List.NonEmpty as NE

import           Ouroboros.Network.Protocol.TxSubmission2.Type (TxSizeInBytes)

import           Ouroboros.Consensus.Block (Point, SlotNo)
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Mempool.Impl.Types
import           Ouroboros.Consensus.Util.IOLike

{-------------------------------------------------------------------------------
  Mempool API
-------------------------------------------------------------------------------}

-- | The mempool is the set of transactions that should be included in the next
-- block.
--
-- In principle this is a /set/ of all the transactions that we receive from our
-- peers. In order to avoid flooding the network with invalid transactions,
-- however, we only want to keep /valid/ transactions in the mempool. That
-- raises the question: valid with respect to which ledger state?
--
-- We opt for a very simple answer to this: the mempool will be interpreted as a
-- /list/ of transactions; which are validated strictly in order, starting from the
-- current ledger state (the current LedgerDB tip). This has a number of
-- advantages:
--
--  * It's simple to implement and it's efficient. In particular, no search for a
--    valid subset is ever required.
--
--  * When producing a block, we can simply take the longest possible prefix of
--    transactions that fits in a block.
--
--  * It supports wallets that submit dependent transactions (where later
--    transaction depends on outputs from earlier ones).
--
-- It has a some drawbacks too:
--
--  * A transaction we discard as invalid on the current ledger state might
--    become valid when validated against some other ledger state we adopt later
--    on.
--
-- However this event is very unlikely and can be solved easily by resubmission of
-- the transaction at the appropriate time.
--
-- We can consider the mempool as an ubiquitous container of transactions that
-- is accessed from multiple places at the same time and should be able to provide
-- a list of transactions (what we call a /snapshot/) which are valid on top
-- a of a given state, possibly on top of the current LedgerDB's tip.
--
-- If the mempool is asked for a snapshot on top of the current LedgerDB's tip, it
-- already has a cached version ready to be provided. If instead it is asked for a
-- snapshot on top of a different ledger state, it will have to revalidate all the
-- /previously valid/ transactions on top of said ledger state.
--
-- When only one thread is operating on the mempool, operations that mutate the
-- state based on the input arguments (tryAddTxs and removeTxs) will produce the
-- same result whether they process transactions one by one or all in one go, so
-- this equality holds:
--
-- > void (tryAddTxs wti txs) === forM_ txs (tryAddTxs wti . (:[]))
-- > void (trAddTxs wti [x,y]) === tryAddTxs wti x >> void (tryAddTxs wti y)
--
-- This shows that @'tryAddTxs' wti@ is an homomorphism from '++' and '>>',
-- which informally makes these operations "distributive".
data Mempool m blk = Mempool {
      -- | Add a bunch of transactions (oldest to newest)
      --
      -- The new transactions provided will be validated, /in order/, against
      -- the Mempool intermediate ledger state. Transactions which are found to
      -- be invalid, with respect to that ledger state, are dropped, whereas
      -- valid transactions are added to the mempool.
      --
      -- Note that transactions that are invalid, with respect to the ledger
      -- state, will /never/ be added to the mempool. However, it is possible
      -- that, at a given point in time, transactions which were once valid but
      -- are now invalid, with respect to the current ledger state, could
      -- /remain/ within the mempool until they are revalidated and dropped from
      -- the mempool via a call to 'syncWithLedger' or by the background thread
      -- that watches the ledger for changes.
      --
      -- This function will return two lists
      --
      -- 1. A list containing the following data:
      --
      --    * Those transactions provided which were found to be valid, as a
      --      'MempoolTxAdded' value. These transactions are now in the Mempool.
      --    * Those transactions provided which were found to be invalid, along
      --      with their accompanying validation errors, as a
      --      'MempoolTxRejected' value. These transactions are not in the
      --      Mempool.
      --
      -- 2. A list containing the transactions that have not yet been added, as
      --    the capacity of the Mempool has been reached. I.e., there is no
      --    space in the Mempool to add the first transaction in this list. Note
      --    that we won't try to add smaller transactions after that first
      --    transaction because they might depend on the first transaction.
      --
      -- POSTCONDITION:
      -- > let prj = \case
      -- >       MempoolTxAdded vtx        -> txForgetValidated vtx
      -- >       MempoolTxRejected tx _err -> tx
      -- > (processed, toProcess) <- tryAddTxs wti txs
      -- > map prj processed ++ toProcess == txs
      --
      -- Note that previously valid transaction that are now invalid with
      -- respect to the current ledger state are dropped from the mempool, but
      -- are not part of the first returned list (nor the second).
      --
      -- In principle it is possible that validation errors are transient; for
      -- example, it is possible that a transaction is rejected because one of
      -- its inputs is not /yet/ available in the UTxO (the transaction it
      -- depends on is not yet in the chain, nor in the mempool). In practice
      -- however it is likely that rejected transactions will still be
      -- rejected later, and should just be dropped.
      --
      -- It is important to note one important special case of transactions
      -- being "invalid": a transaction will /also/ be considered invalid if
      -- /that very same transaction/ is already included on the blockchain
      -- (after all, by definition that must mean its inputs have been used).
      -- Rejected transactions are therefore not necessarily a sign of
      -- malicious behaviour. Indeed, we would expect /most/ transactions that
      -- are reported as invalid by 'tryAddTxs' to be invalid precisely
      -- because they have already been included. Distinguishing between these
      -- two cases can be done in theory, but it is expensive unless we have
      -- an index of transaction hashes that have been included on the
      -- blockchain.
      tryAddTxs      :: WhetherToIntervene
                     -> [GenTx blk]
                     -> m ( [MempoolAddTxResult blk]
                          , [GenTx blk]
                          )

      -- | Manually remove the given transactions from the mempool.
    , removeTxs      :: NE.NonEmpty (GenTxId blk) -> m ()

      -- | Sync the transactions in the mempool with the current ledger state
      --  at the tip of the 'ChainDB'.
      --
      -- The transactions that exist within the mempool will be revalidated
      -- against the current ledger state. Transactions which are found to be
      -- invalid with respect to the current ledger state, will be dropped
      -- from the mempool, whereas valid transactions will remain.
      --
      -- We keep this in @m@ instead of @STM m@ to leave open the possibility
      -- of persistence. Additionally, this makes it possible to trace the
      -- removal of invalid transactions.
      --
      -- n.b. in our current implementation, when one opens a mempool, we
      -- spawn a thread which performs this action whenever the 'ChainDB' tip
      -- point changes.
    , syncWithLedger :: m (MempoolSnapshot blk)

      -- | Get a snapshot of the current mempool state. This allows for
      -- further pure queries on the snapshot.
      --
      -- This doesn't look at the ledger state at all, i.e. it produces a
      -- snapshot from the current InternalState of the mempool.
    , getSnapshot    :: STM m (MempoolSnapshot blk)

      -- | Get a snapshot of the mempool state that is valid with respect to
      -- the given ticked ledger state.
      --
      -- This does not update the internal state of the mempool.
      --
    , getSnapshotFor ::
           Point blk
        -> SlotNo
        -> TickedLedgerState blk DiffMK
        -> m (Maybe (MempoolSnapshot blk))

      -- | Get the mempool's capacity in bytes.
      --
      -- Note that the capacity of the Mempool, unless it is overridden with
      -- 'MempoolCapacityBytesOverride', can dynamically change when the
      -- ledger state is updated: it will be set to twice the current ledger's
      -- maximum transaction capacity of a block.
      --
      -- When the capacity happens to shrink at some point, we /do not/ remove
      -- transactions from the Mempool to satisfy this new lower limit.
      -- Instead, we treat it the same way as a Mempool which is /at/
      -- capacity, i.e., we won't admit new transactions until some have been
      -- removed because they have become invalid.
    , getCapacity    :: STM m MempoolCapacityBytes

      -- | Return the post-serialisation size in bytes of a 'GenTx'.
    , getTxSize      :: GenTx blk -> TxSizeInBytes
    }

-- | Wrapper around 'implTryAddTxs' that blocks until all transaction have
-- either been added to the Mempool or rejected.
--
-- This function does not sync the Mempool contents with the ledger state in
-- case the latter changes, it relies on the background thread to do that.
--
-- See the necessary invariants on the Haddock for 'tryAddTxs'.
addTxs
  :: forall m blk. MonadSTM m
  => Mempool m blk
  -> [GenTx blk]
  -> m [MempoolAddTxResult blk]
addTxs mempool = addTxsHelper mempool DoNotIntervene

-- | Variation on 'addTxs' that is more forgiving when possible
--
-- See 'Intervene'.
addLocalTxs
  :: forall m blk. MonadSTM m
  => Mempool m blk
  -> [GenTx blk]
  -> m [MempoolAddTxResult blk]
addLocalTxs mempool = addTxsHelper mempool Intervene

-- | See 'addTxs'
addTxsHelper
  :: forall m blk. MonadSTM m
  => Mempool m blk
  -> WhetherToIntervene
  -> [GenTx blk]
  -> m [MempoolAddTxResult blk]
addTxsHelper mempool wti = \txs -> do
    (processed, toAdd) <- tryAddTxs mempool wti txs
    case toAdd of
      [] -> return processed
      _  -> go [processed] toAdd
  where
    go
      :: [[MempoolAddTxResult blk]]
         -- ^ The outer list is in reverse order, but all the inner lists will
         -- be in the right order.
      -> [GenTx blk]
      -> m [MempoolAddTxResult blk]
    go acc []         = return (concat (reverse acc))
    go acc txs@(tx:_) = do
      let firstTxSize = getTxSize mempool tx
      -- Wait until there's at least room for the first transaction we're
      -- trying to add, otherwise there's no point in trying to add it.
      atomically $ do
        curSize <- msNumBytes . snapshotMempoolSize <$> getSnapshot mempool
        MempoolCapacityBytes capacity <- getCapacity mempool
        check (curSize + firstTxSize <= capacity)
      -- It is possible that between the check above and the call below, other
      -- transactions are added, stealing our spot, but that's fine, we'll
      -- just recurse again without progress.
      (added, toAdd) <- tryAddTxs mempool wti txs
      go (added:acc) toAdd

