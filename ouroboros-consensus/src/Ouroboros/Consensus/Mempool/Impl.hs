{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Mempool.Impl (
    openMempool
  , LedgerInterface (..)
  , MempoolCapacity (..)
  , chainDBLedgerInterface
  , txsToMempoolSize
  , TicketNo
    -- * For testing purposes
  , openMempoolWithoutSyncThread
  ) where

import           Control.Exception (assert)
import           Control.Monad.Except
import qualified Data.Foldable as Foldable
import qualified Data.Set as Set
import           Data.Typeable
import           Data.Word (Word32)
import           GHC.Generics (Generic)

import           Control.Tracer

import           Ouroboros.Network.Block (ChainHash, Point, SlotNo,
                     StandardHash)
import qualified Ouroboros.Network.Block as Block
import           Ouroboros.Network.Point (WithOrigin (..))

import           Ouroboros.Storage.ChainDB (ChainDB)
import qualified Ouroboros.Storage.ChainDB.API as ChainDB

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Mempool.TxSeq (TicketNo, TxSeq (..),
                     TxTicket (..), fromTxSeq, lookupByTicketNo,
                     splitAfterTicketNo, splitAfterTxSize, zeroTicketNo)
import qualified Ouroboros.Consensus.Mempool.TxSeq as TxSeq
import           Ouroboros.Consensus.Util (repeatedly)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Consensus.Util.STM (onEachChange)

{-------------------------------------------------------------------------------
  Top-level API
-------------------------------------------------------------------------------}

openMempool :: (IOLike m, ApplyTx blk)
            => ResourceRegistry m
            -> LedgerInterface m blk
            -> LedgerConfig blk
            -> MempoolCapacity
            -> Tracer m (TraceEventMempool blk)
            -> m (Mempool m blk TicketNo)
openMempool registry ledger cfg capacity tracer = do
    env <- initMempoolEnv ledger cfg capacity tracer
    forkSyncStateOnTipPointChange registry env
    return $ mkMempool env

-- | Unlike 'openMempool', this function does not fork a background thread
-- that synchronises with the ledger state whenever the later changes.
--
-- Intended for testing purposes.
openMempoolWithoutSyncThread
  :: (IOLike m, ApplyTx blk)
  => LedgerInterface m blk
  -> LedgerConfig blk
  -> MempoolCapacity
  -> Tracer m (TraceEventMempool blk)
  -> m (Mempool m blk TicketNo)
openMempoolWithoutSyncThread ledger cfg capacity tracer =
    mkMempool <$> initMempoolEnv ledger cfg capacity tracer

mkMempool :: (IOLike m, ApplyTx blk)
          => MempoolEnv m blk -> Mempool m blk TicketNo
mkMempool env = Mempool
    { addTxs        = implAddTxs env []
    , removeTxs     = implRemoveTxs env
    , withSyncState = implWithSyncState env
    , getSnapshot   = implGetSnapshot env
    , zeroIdx       = zeroTicketNo
    }

-- | Abstract interface needed to run a Mempool.
data LedgerInterface m blk = LedgerInterface
  { getCurrentLedgerState :: STM m (LedgerState blk)
  }

-- | Represents the maximum number of bytes worth of transactions that a
-- 'Mempool' can contain.
newtype MempoolCapacity = MempoolCapacity Word32
  deriving (Show)

-- | Create a 'LedgerInterface' from a 'ChainDB'.
chainDBLedgerInterface :: IOLike m => ChainDB m blk -> LedgerInterface m blk
chainDBLedgerInterface chainDB = LedgerInterface
    { getCurrentLedgerState = ledgerState <$> ChainDB.getCurrentLedger chainDB
    }

{-------------------------------------------------------------------------------
  Internal state
-------------------------------------------------------------------------------}

-- | Internal state in the mempool
data InternalState blk = IS {
      -- | Transactions currently in the mempool
      isTxs          :: !(TxSeq (GenTx blk))

      -- | The tip of the chain that 'isTxs' was validated against
    , isTip          :: !(ChainHash blk)

      -- | The mempool 'TicketNo' counter.
      --
      -- See 'vrLastTicketNo' for more information.
    , isLastTicketNo :: !TicketNo
    }
  deriving (Generic)

deriving instance ( NoUnexpectedThunks (GenTx blk)
                  , StandardHash blk
                  , Typeable blk
                  ) => NoUnexpectedThunks (InternalState blk)

data MempoolEnv m blk = MempoolEnv {
      mpEnvLedger    :: LedgerInterface m blk
    , mpEnvLedgerCfg :: LedgerConfig blk
    , mpEnvCapacity  :: !MempoolCapacity
    , mpEnvStateVar  :: StrictTVar m (InternalState blk)
    , mpEnvTracer    :: Tracer m (TraceEventMempool blk)
    }

initInternalState :: InternalState blk
initInternalState = IS TxSeq.Empty Block.GenesisHash zeroTicketNo

initMempoolEnv :: (IOLike m, ApplyTx blk)
               => LedgerInterface m blk
               -> LedgerConfig blk
               -> MempoolCapacity
               -> Tracer m (TraceEventMempool blk)
               -> m (MempoolEnv m blk)
initMempoolEnv ledgerInterface cfg capacity tracer = do
    isVar <- newTVarM initInternalState
    return MempoolEnv
      { mpEnvLedger    = ledgerInterface
      , mpEnvLedgerCfg = cfg
      , mpEnvCapacity  = capacity
      , mpEnvStateVar  = isVar
      , mpEnvTracer    = tracer
      }

-- | Spawn a thread which syncs the 'Mempool' state whenever the 'LedgerState'
-- changes.
forkSyncStateOnTipPointChange :: forall m blk. (IOLike m, ApplyTx blk)
                              => ResourceRegistry m
                              -> MempoolEnv m blk
                              -> m ()
forkSyncStateOnTipPointChange registry menv =
    onEachChange registry id Nothing getCurrentTip action
  where
    action :: Point blk -> m ()
    action _tipPoint = implWithSyncState menv TxsForUnknownBlock $ \_txs ->
                         return ()

    -- Using the tip ('Point') allows for quicker equality checks
    getCurrentTip :: STM m (Point blk)
    getCurrentTip = ledgerTipPoint <$> getCurrentLedgerState (mpEnvLedger menv)

{-------------------------------------------------------------------------------
  Mempool Implementation
-------------------------------------------------------------------------------}

-- | Add a bunch of transactions (oldest to newest)
--
-- If the mempool capacity is reached, this function will block until it's
-- able to at least attempt validating and adding each of the provided
-- transactions to the mempool.
--
-- Steps taken by this function (much of this information can also be found
-- in comments throughout the code):
--
-- * Attempt to sync the mempool with the ledger state, removing transactions
--   from the mempool as necessary.
--
--   In the event that some work is done here, we should update the mempool
--   state and commit the STM transaction. From the STM transaction, we'll
--   return the provided transactions which weren't yet validated (all of
--   them) and 'implAddTxs' will call itself given these remaining unvalidated
--   transactions.
--
--   If the sync resulted in no work being done, we don't have to worry about
--   losing any changes in the event of a 'retry'. So we continue by calling
--   'validateNew', providing the new transactions as an argument.
--
-- * In 'validateNew', we first attempt to individually validate the first
--   transaction of the provided list.
--
--   If this is successful and we haven't reached the mempool capacity, we
--   continue by attempting to validate each of the remaining new transactions
--   with the function 'validateNewUntilMempoolFull'.
--
--   If this fails due to the mempool capacity being reached, we 'retry' the
--   STM transaction. We've done very little work up to this point so this
--   is quite cheap.
--
-- * In 'validateNewUntilMempoolFull', we attempt to recursively validate and
--   add each of the provided transactions, one-by-one. If at any point the
--   mempool capacity is reached, we return the last 'ValidationResult' along
--   with the remaining unvalidated transactions (those which we weren't able
--   to attempt yet).
--
-- * Given the result from 'validateNewUntilMempoolFull', 'validateNew'
--   updates the mempool state and returns the same result back up to
--   'implAddTxs'.
--
-- * Given the result from 'validateNew', commit the STM transaction and
--   'implAddTxs' checks whether there are any remaining transactions which
--   are yet to be validated. If no transactions remain, we return the result.
--   On the other hand, if there are still remaining transactions,
--   'implAddTxs' calls itself given an accumulation of its results thus far
--   along with the remaining transactions.
implAddTxs :: forall m blk. (IOLike m, ApplyTx blk)
           => MempoolEnv m blk
           -> [(GenTx blk, Maybe (ApplyTxErr blk))]
           -- ^ An accumulator of the results from each call to 'implAddTxs'.
           --
           -- Because this function will recurse until it's able to at least
           -- attempt validating and adding each transaction provided, we keep
           -- this accumulator of the results. 'implAddTxs' will recurse in
           -- the event that it wasn't able to attempt adding all of the
           -- provided transactions due to the mempool being at its capacity.
           -> [GenTx blk]
           -- ^ Transactions to validate and add to the mempool.
           -> m [(GenTx blk, Maybe (ApplyTxErr blk))]
implAddTxs _     _     []  = pure []
implAddTxs mpEnv accum txs = assert (all txInvariant txs) $ do
    (vr, removed, rejected, unvalidated, mempoolSize) <- atomically $ do
      IS{isTip = initialISTip} <- readTVar mpEnvStateVar

      -- First sync the state, which might remove some transactions
      syncRes@ValidationResult
        { vrBefore
        , vrValid
        , vrInvalid = removed
        , vrLastTicketNo
        } <- validateIS mpEnv TxsForUnknownBlock

      -- Determine whether the tip was updated after a call to 'validateIS'
      --
      -- If the tip was updated, instead of immediately going on to call
      -- 'validateNew' which can potentially 'retry', we should commit this
      -- STM transaction to ensure that we don't lose any of the changes
      -- brought about by 'validateIS' thus far.
      --
      -- On the other hand, if the tip wasn't updated, we don't have to worry
      -- about losing any changes in the event that we have to 'retry' this
      -- STM transaction. So we should continue by validating the provided new
      -- transactions.
      if initialISTip /= vrBefore
        then do
          -- The tip changed.
          -- Because 'validateNew' can 'retry', we'll commit this STM
          -- transaction here to ensure that we don't lose any of the changes
          -- brought about by 'validateIS' thus far.
          writeTVar mpEnvStateVar IS { isTxs          = vrValid
                                     , isTip          = vrBefore
                                     , isLastTicketNo = vrLastTicketNo
                                     }
          mempoolSize <- getMempoolSize mpEnv
          pure (syncRes, removed, [], txs, mempoolSize)
        else do
          -- The tip was unchanged.
          -- Therefore, we don't have to worry about losing any changes in the
          -- event that we have to 'retry' this STM transaction. Continue by
          -- validating the provided new transactions.
          (vr, unvalidated) <- validateNew syncRes
          mempoolSize       <- getMempoolSize mpEnv
          pure (vr, removed, vrInvalid vr, unvalidated, mempoolSize)

    let ValidationResult { vrNewValid = accepted } = vr

    -- We record the time at which the transactions were added to the mempool
    -- so we can use it in our performance measurements.
    time <- getMonotonicTime

    traceBatch TraceMempoolRemoveTxs   mempoolSize (map fst removed) time
    traceBatch TraceMempoolAddTxs      mempoolSize accepted          time
    traceBatch TraceMempoolRejectedTxs mempoolSize rejected          time

    case unvalidated of
      -- All of the provided transactions have been validated.
      [] -> return (mkRes accum accepted rejected)

      -- There are still transactions that remain which need to be validated.
      _  -> implAddTxs mpEnv (mkRes accum accepted rejected) unvalidated
  where
    MempoolEnv
      { mpEnvStateVar
      , mpEnvTracer
      , mpEnvLedgerCfg
      , mpEnvCapacity = MempoolCapacity mempoolCap
      } = mpEnv

    traceBatch mkEv size batch time
      | null batch = return ()
      | otherwise  = traceWith mpEnvTracer (mkEv batch size time)

    mkRes acc accepted rejected =
         [(tx, Just err) | (tx, err) <- rejected]
      ++ zip accepted (repeat Nothing)
      ++ acc

    -- | Attempt to validate and add as many new transactions to the mempool as
    -- possible, returning the last 'ValidationResult' and the remaining
    -- transactions which couldn't be added due to the mempool capacity being
    -- reached.
    validateNew :: ValidationResult blk
                -> STM m (ValidationResult blk, [GenTx blk])
    validateNew res =
        let res' = res { vrInvalid = [] }
        in case txs of
          []                  -> return (res', [])
          headTx:remainingTxs -> do
            -- First, attempt to individually validate the first new transaction.
            --
            -- If this is successful, we should continue to validate all of the
            -- other new transactions one-by-one. If the mempool capacity would be
            -- reached at any step, we update the 'InternalState' with the work
            -- that we've already done and return the last 'ValidationResult' along
            -- with the remaining unvalidated transactions.
            --
            -- If, however, this fails due to the mempool capacity being met, we
            -- should simply 'retry' as it will be cheap due to the fact that
            -- we've done very little work in this STM transaction.
            --
            -- It makes sense to do this due to the fact that a 'retry' at this
            -- point is likely to be more efficient than simply returning the
            -- result and constantly recursing until there's at least one space in
            -- the mempool (if remaining unvalidated transactions are returned up
            -- to 'implAddTxs', 'implAddTxs' will recurse).
            headTxValidationRes <- validateNewUntilMempoolFull [headTx] res'
            case headTxValidationRes of
              -- Mempool capacity hasn't been reached (no remaining unvalidated
              -- transactions were returned).
              (vr, []) -> do
                -- Continue validating the remaining transactions.
                (vr', unvalidatedTxs) <- validateNewUntilMempoolFull remainingTxs vr
                writeTVar mpEnvStateVar IS { isTxs          = vrValid        vr'
                                           , isTip          = vrBefore       vr'
                                           , isLastTicketNo = vrLastTicketNo vr'
                                           }
                pure (vr', unvalidatedTxs)

              -- The mempool capacity has been reached.
              _ -> retry

    validateNewUntilMempoolFull
      :: [GenTx blk]
      -- ^ The new transactions to validate.
      -> ValidationResult blk
      -- ^ The 'ValidationResult' from which to begin validating.
      -> STM m (ValidationResult blk, [GenTx blk])
      -- ^ The last 'ValidationResult' along with the remaining transactions
      -- (those not yet validated due to the mempool capacity being reached).
    validateNewUntilMempoolFull []      vr = pure (vr, [])
    validateNewUntilMempoolFull (t:ts)  vr = do
      MempoolSize { msNumBytes } <- getMempoolSize mpEnv
      let newValidSizeInBytes = Foldable.foldl'
            (\acc tx -> acc + txSize tx)
            0
            (vrNewValid vr)
      -- The size of a mempool should never be greater than its capacity.
      assert (msNumBytes <= mempoolCap) $
        -- Here, we check whether we're at the mempool's capacity /before/
        -- attempting to validate the next transaction.
        if (msNumBytes + newValidSizeInBytes + txSize t) <= mempoolCap
          then validateNewUntilMempoolFull ts (extendVRNew mpEnvLedgerCfg t vr)
          else pure (vr, t:ts) -- if we're at mempool capacity, we return the
                               -- last 'ValidationResult' as well as the
                               -- remaining transactions (those not yet
                               -- validated).

implRemoveTxs
  :: (IOLike m, ApplyTx blk)
  => MempoolEnv m blk
  -> [GenTxId blk]
  -> m ()
implRemoveTxs mpEnv@MempoolEnv{mpEnvTracer, mpEnvStateVar} txIds = do
    (removed, mempoolSize) <- atomically $ do
      -- Filtering is O(n), but this function will so rarely be used, as it is
      -- an escape hatch when there's an inconsistency between the ledger and
      -- the mempool.
      modifyTVar mpEnvStateVar $ \is@IS{isTxs} -> is
        { isTxs = TxSeq.filterTxs
            (\TxTicket { txTicketTx } -> txId txTicketTx `notElem` toRemove)
            isTxs
        }
      -- TODO some duplication with 'implWithSyncState'
      ValidationResult
        { vrBefore
        , vrValid
        , vrInvalid
        , vrLastTicketNo
        } <- validateIS mpEnv TxsForUnknownBlock
      writeTVar mpEnvStateVar IS
        { isTxs          = vrValid
        , isTip          = vrBefore
        , isLastTicketNo = vrLastTicketNo
        }
      -- The size of the mempool /after/ manually removing the transactions.
      mempoolSize <- getMempoolSize mpEnv
      return (map fst vrInvalid, mempoolSize)
    unless (null txIds) $
      traceWith mpEnvTracer $
        TraceMempoolManuallyRemovedTxs txIds removed mempoolSize
  where
    toRemove = Set.fromList txIds

implWithSyncState
  :: (IOLike m, ApplyTx blk)
  => MempoolEnv m blk
  -> BlockSlot
  -> (MempoolSnapshot blk TicketNo -> STM m a)
  -> m a
implWithSyncState mpEnv@MempoolEnv{mpEnvTracer, mpEnvStateVar} blockSlot f = do
    (removed, mempoolSize, res) <- atomically $ do
      ValidationResult
        { vrBefore
        , vrValid
        , vrInvalid
        , vrLastTicketNo
        } <- validateIS mpEnv blockSlot
      writeTVar mpEnvStateVar IS
        { isTxs          = vrValid
        , isTip          = vrBefore
        , isLastTicketNo = vrLastTicketNo
        }
      -- The size of the mempool /after/ removing invalid transactions.
      mempoolSize <- getMempoolSize mpEnv
      snapshot    <- implGetSnapshot mpEnv
      res         <- f snapshot
      return (map fst vrInvalid, mempoolSize, res)
    unless (null removed) $ do
      time <- getMonotonicTime
      traceWith mpEnvTracer $ TraceMempoolRemoveTxs removed mempoolSize time
    return res

implGetSnapshot :: IOLike m
                => MempoolEnv m blk
                -> STM m (MempoolSnapshot blk TicketNo)
implGetSnapshot MempoolEnv{mpEnvStateVar} = do
  is <- readTVar mpEnvStateVar
  pure MempoolSnapshot
    { snapshotTxs        = implSnapshotGetTxs        is
    , snapshotTxsAfter   = implSnapshotGetTxsAfter   is
    , snapshotTxsForSize = implSnapshotGetTxsForSize is
    , snapshotLookupTx   = implSnapshotGetTx         is
    }

-- | Return the number of transactions in the Mempool paired with their total
-- size in bytes.
getMempoolSize :: (IOLike m, ApplyTx blk)
               => MempoolEnv m blk
               -> STM m MempoolSize
getMempoolSize MempoolEnv{mpEnvStateVar} =
    txsToMempoolSize . isTxs <$> readTVar mpEnvStateVar

-- | Given a 'Foldable' of transactions, calculate what the 'MempoolSize'
-- would be if a mempool were to consist /only/ of those transactions.
txsToMempoolSize :: (Foldable t, ApplyTx blk) => t (GenTx blk) -> MempoolSize
txsToMempoolSize = foldMap toMempoolSize
  where
    toMempoolSize :: ApplyTx blk => GenTx blk -> MempoolSize
    toMempoolSize tx = MempoolSize { msNumTxs = 1, msNumBytes = txSize tx }

{-------------------------------------------------------------------------------
  MempoolSnapshot Implementation
-------------------------------------------------------------------------------}

implSnapshotGetTxs :: InternalState blk
                   -> [(GenTx blk, TicketNo)]
implSnapshotGetTxs = (flip implSnapshotGetTxsAfter) zeroTicketNo

implSnapshotGetTxsAfter :: InternalState blk
                        -> TicketNo
                        -> [(GenTx blk, TicketNo)]
implSnapshotGetTxsAfter IS{isTxs} tn =
    fromTxSeq $ snd $ splitAfterTicketNo isTxs tn

implSnapshotGetTxsForSize :: InternalState blk
                          -> Word32
                          -> [(GenTx blk, TicketNo)]
implSnapshotGetTxsForSize IS{isTxs} maxSize =
    fromTxSeq $ fst $ splitAfterTxSize isTxs maxSize

implSnapshotGetTx :: InternalState blk
                  -> TicketNo
                  -> Maybe (GenTx blk)
implSnapshotGetTx IS{isTxs} tn = isTxs `lookupByTicketNo` tn

{-------------------------------------------------------------------------------
  Validation
-------------------------------------------------------------------------------}

data ValidationResult blk = ValidationResult {
    -- | The tip of the chain before applying these transactions
    vrBefore       :: ChainHash blk

    -- | The transactions that were found to be valid (oldest to newest)
  , vrValid        :: TxSeq (GenTx blk)

    -- | New transactions (not previously known) which were found to be valid.
    --
    -- n.b. This will only contain valid transactions which were /newly/ added
    -- to the mempool (not previously known valid transactions).
    --
    -- Order not guaranteed.
  , vrNewValid     :: [GenTx blk]

    -- | The state of the ledger after 'vrValid'
    --
    -- NOTE: This is intentionally not a strict field, so that we don't
    -- evaluate the final ledger state if we don't have to.
  , vrAfter        :: TickedLedgerState blk

    -- | The transactions that were invalid, along with their errors
    --
    -- Order not guaranteed
  , vrInvalid      :: [(GenTx blk, ApplyTxErr blk)]

    -- | The mempool 'TicketNo' counter.
    --
    -- When validating new transactions, this should be incremented, starting
    -- from 'isLastTicketNo' of the 'InternalState'.
    -- When validating previously applied transactions, this field should not
    -- be affected.
  , vrLastTicketNo :: TicketNo
  }

-- | Initialize 'ValidationResult' from a ledger state and a list of
-- transactions /known/ to be valid in that ledger state
initVR :: forall blk. ApplyTx blk
       => LedgerConfig blk
       -> TxSeq (GenTx blk)
       -> TickedLedgerState blk
       -> TicketNo
       -> ValidationResult blk
initVR cfg = \knownValid st lastTicketNo -> ValidationResult {
      vrBefore       = ledgerTipHash (getTickedLedgerState st)
    , vrValid        = knownValid
    , vrNewValid     = []
    , vrAfter        = afterKnownValid
                         (Foldable.toList knownValid)
                         st
    , vrInvalid      = []
    , vrLastTicketNo = lastTicketNo
    }
  where
    afterKnownValid :: [GenTx blk]
                    -> TickedLedgerState blk -> TickedLedgerState blk
    afterKnownValid []       = id
    afterKnownValid (tx:txs) = afterKnownValid txs . reapplyTxSameState cfg tx

-- | Extend 'ValidationResult' with a previously validated transaction that
-- may or may not be valid in this ledger state
--
-- n.b. Even previously validated transactions may not be valid in a different
-- ledger state;  it is /still/ useful to indicate whether we have previously
-- validated this transaction because, if we have, we can utilize 'reapplyTx'
-- rather than 'applyTx' and, therefore, skip things like cryptographic
-- signatures.
extendVRPrevApplied :: ApplyTx blk
                    => LedgerConfig blk
                    -> (GenTx blk, TicketNo)
                    -> ValidationResult blk
                    -> ValidationResult blk
extendVRPrevApplied cfg (tx, tn)
         vr@ValidationResult{vrValid, vrAfter, vrInvalid} =
    case runExcept (reapplyTx cfg tx vrAfter) of
      Left err  -> vr { vrInvalid = (tx, err) : vrInvalid
                      }
      Right st' -> vr { vrValid   = vrValid :> TxTicket tx tn (txSize tx)
                      , vrAfter   = st'
                      }

-- | Extend 'ValidationResult' with a new transaction (one which we have not
-- previously validated) that may or may not be valid in this ledger state.
extendVRNew :: ApplyTx blk
            => LedgerConfig blk
            -> GenTx blk
            -> ValidationResult blk
            -> ValidationResult blk
extendVRNew cfg tx
         vr@ValidationResult { vrValid
                             , vrAfter
                             , vrInvalid
                             , vrLastTicketNo
                             , vrNewValid
                             } =
    let nextTicketNo = succ vrLastTicketNo
    in  case runExcept (applyTx cfg tx vrAfter) of
      Left err  -> vr { vrInvalid      = (tx, err) : vrInvalid
                      }
      Right st' -> vr { vrValid        = vrValid :> TxTicket tx nextTicketNo (txSize tx)
                      , vrNewValid     = tx : vrNewValid
                      , vrAfter        = st'
                      , vrLastTicketNo = nextTicketNo
                      }

-- | Validate internal state
validateIS :: forall m blk. (IOLike m, ApplyTx blk)
           => MempoolEnv m blk -> BlockSlot -> STM m (ValidationResult blk)
validateIS MempoolEnv{mpEnvLedger, mpEnvLedgerCfg, mpEnvStateVar} blockSlot =
    go <$> getCurrentLedgerState mpEnvLedger
       <*> readTVar mpEnvStateVar
  where
    go :: LedgerState      blk
       -> InternalState    blk
       -> ValidationResult blk
    go st IS{isTxs, isTip, isLastTicketNo}
        | ledgerTipHash (getTickedLedgerState st') == isTip
        = initVR mpEnvLedgerCfg isTxs st' isLastTicketNo
        | otherwise
        = repeatedly (extendVRPrevApplied mpEnvLedgerCfg) (fromTxSeq isTxs)
        $ initVR mpEnvLedgerCfg TxSeq.Empty st' isLastTicketNo
      where
        st' :: TickedLedgerState blk
        st' = applyChainTick mpEnvLedgerCfg slot st

        -- If we don't yet know the slot number, optimistically assume that they
        -- will be included in a block in the next available slot
        slot :: SlotNo
        slot = case blockSlot of
                 TxsForBlockInSlot s -> s
                 TxsForUnknownBlock  ->
                   case ledgerTipSlot st of
                     -- TODO: 'genesisSlotNo' is badly named. This is the slot
                     -- number of the first real block.
                     Origin -> Block.genesisSlotNo
                     At s   -> succ s
