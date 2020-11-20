{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}

module Ouroboros.Consensus.Mempool.API (
    Mempool(..)
  , ForgeLedgerState(..)
  , MempoolCapacityBytes (..)
  , MempoolSnapshot(..)
  , MempoolAddTxResult (..)
  , isMempoolTxAdded
  , isMempoolTxRejected
  , MempoolSize (..)
  , TraceEventMempool(..)
    -- * Re-exports
  , TxSizeInBytes
  , TicketNo
  , MempoolCapacityBytesOverride(..)
  , openMempool
  , openMempoolWithoutSyncThread
  ) where

import           Control.Tracer (Tracer)

import           Ouroboros.Network.Protocol.TxSubmission.Type (TxSizeInBytes)

import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Mempool.Data
import           Ouroboros.Consensus.Mempool.TxSeq
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry)

import           Ouroboros.Consensus.Mempool.Impl

-- | Mempool
--
-- The mempool is the set of transactions that should be included in the next
-- block. In principle this is a /set/ of all the transactions that we receive
-- from our peers. In order to avoid flooding the network with invalid
-- transactions,  however, we only want to keep /valid/ transactions in the
-- mempool. That raises the question: valid with respect to which ledger state?
--
-- We opt for a very simple answer to this: the mempool will be interpreted
-- as a /list/ of transactions; which are validated strictly in order, starting
-- from the current ledger state. This has a number of advantages:
--
-- * It's simple to implement and it's efficient. In particular, no search for
--   a valid subset is ever required.
-- * When producing a block, we can simply take the longest possible prefix
--   of transactions that fits in a block.
-- * It supports wallets that submit dependent transactions (where later
--   transaction depends on outputs from earlier ones).
data Mempool m blk idx = Mempool {
      -- | Add a bunch of transactions (oldest to newest)
      --
      -- As long as we keep the mempool entirely in-memory this could live in
      -- @STM m@; we keep it in @m@ instead to leave open the possibility of
      -- persistence.
      --
      -- The new transactions provided will be validated, /in order/, against
      -- the ledger state obtained by applying all the transactions already in
      -- the Mempool to it. Transactions which are found to be invalid, with
      -- respect to the ledger state, are dropped, whereas valid transactions
      -- are added to the mempool.
      --
      -- Note that transactions that are invalid, with respect to the ledger
      -- state, will /never/ be added to the mempool. However, it is possible
      -- that, at a given point in time, transactions which were once valid
      -- but are now invalid, with respect to the current ledger state, could
      -- exist within the mempool until they are revalidated and dropped from
      -- the mempool via a call to 'syncWithLedger' or by the background
      -- thread that watches the ledger for changes.
      --
      -- This function will return two lists
      --
      -- 1. A list containing the following transactions:
      --
      --    * Those transactions provided which were found to be valid, along
      --      with 'MempoolTxAdded' for their accompanying
      --      'MempoolAddTxResult' values. These transactions are now in the
      --      Mempool.
      --    * Those transactions provided which were found to be invalid,
      --      along with their accompanying validation errors. These
      --      transactions are not in the Mempool.
      --
      -- 2. A list containing the transactions that have not yet been added
      --    yet, as the capacity of the Mempool has been reached. I.e., there
      --    is no space in the Mempool to add the first transaction in this
      --    list. Note that we won't try to add smaller transactions after
      --    that first transaction because they might depend on the first
      --    transaction.
      --
      -- POSTCONDITION:
      -- > (processed, toProcess) <- tryAddTxs txs
      -- > map fst processed ++ toProcess == txs
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
      --
      tryAddTxs      :: [GenTx blk]
                     -> m ( [(GenTx blk, MempoolAddTxResult blk)]
                          , [GenTx blk]
                          )

      -- | Wrapper around 'implTryAddTxs' that blocks until all transaction have
      -- either been added to the Mempool or rejected.
      --
      -- This function does not sync the Mempool contents with the ledger state in
      -- case the latter changes, it relies on the background thread to do that.
      --
      -- POSTCONDITON:
      -- > processed <- addTxs mpEnv txs
      -- > map fst processed == txs
      --

    , addTxsBlock :: MonadSTM m
        => [GenTx blk]
        -> m [(GenTx blk, MempoolAddTxResult blk)]

      -- | Manually remove the given transactions from the mempool.
    , removeTxs      :: [GenTxId blk] -> m ()

      -- | Sync the transactions in the mempool with the current ledger state
      --  of the 'ChainDB'.
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
    , syncWithLedger :: m (MempoolSnapshot blk idx)

      -- | Get a snapshot of the current mempool state. This allows for
      -- further pure queries on the snapshot.
      --
      -- This doesn't look at the ledger state at all.
    , getSnapshot    :: STM m (MempoolSnapshot blk idx)

      -- | Get a snapshot of the mempool state that is valid with respect to
      -- the given ledger state
      --
      -- This does not update the state of the mempool.
    , getSnapshotFor :: ForgeLedgerState blk -> STM m (MempoolSnapshot blk idx)

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

      -- | Represents the initial value at which the transaction ticket number
      -- counter will start (i.e. the zeroth ticket number).
    , zeroIdx        :: idx
    }


mkMempool
  :: ( IOLike m
     , LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     , ValidateEnvelope blk
     )
  => MempoolEnv m blk -> Mempool m blk TicketNo
mkMempool env = Mempool
    { tryAddTxs      = implTryAddTxs      env
    , addTxsBlock    = implAddTxsBlock   env
    , removeTxs      = implRemoveTxs      env
    , syncWithLedger = implSyncWithLedger env
    , getSnapshot    = implGetSnapshot    env
    , getSnapshotFor = implGetSnapshotFor env
    , getCapacity    = implGetCapacity    env
    , getTxSize      = mpEnvTxSize        env
    , zeroIdx        = zeroTicketNo
    }


{-------------------------------------------------------------------------------
  Top-level API
-------------------------------------------------------------------------------}

openMempool
  :: ( IOLike m
     , LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     , ValidateEnvelope blk
     )
  => ResourceRegistry m
  -> LedgerInterface m blk
  -> LedgerConfig blk
  -> MempoolCapacityBytesOverride
  -> Tracer m (TraceEventMempool blk)
  -> (GenTx blk -> TxSizeInBytes)
  -> m (Mempool m blk TicketNo)
openMempool registry ledger cfg capacityOverride tracer txSize = do
    env <- initMempoolEnv ledger cfg capacityOverride tracer txSize
    forkSyncStateOnTipPointChange registry env
    return $ mkMempool env

-- | Unlike 'openMempool', this function does not fork a background thread
-- that synchronises with the ledger state whenever the later changes.
--
-- Intended for testing purposes.
openMempoolWithoutSyncThread
  :: ( IOLike m
     , LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     , ValidateEnvelope blk
     )
  => LedgerInterface m blk
  -> LedgerConfig blk
  -> MempoolCapacityBytesOverride
  -> Tracer m (TraceEventMempool blk)
  -> (GenTx blk -> TxSizeInBytes)
  -> m (Mempool m blk TicketNo)
openMempoolWithoutSyncThread ledger cfg capacityOverride tracer txSize =
    mkMempool <$> initMempoolEnv ledger cfg capacityOverride tracer txSize
