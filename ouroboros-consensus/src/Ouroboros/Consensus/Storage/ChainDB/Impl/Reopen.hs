{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wredundant-constraints #-}

-- | Closing and reopening
module Ouroboros.Consensus.Storage.ChainDB.Impl.Reopen
  ( isOpen
  , closeDB
  , reopen
    -- * Auxiliary
  , pointToEpoch
  ) where

import           Control.Monad (when)
import           Control.Tracer
import           Data.Functor ((<&>))
import           GHC.Stack (HasCallStack)

import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (pattern BlockPoint,
                     pattern GenesisPoint, HasHeader (..), Point, castPoint)

import           Ouroboros.Consensus.Block (Header)
import           Ouroboros.Consensus.BlockchainTime (getCurrentSlot)
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Util (whenJust)
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Consensus.Storage.Common (EpochNo)
import           Ouroboros.Consensus.Storage.EpochInfo (EpochInfo,
                     epochInfoEpoch)

import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Background as Background
import           Ouroboros.Consensus.Storage.ChainDB.Impl.ChainSel
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.ImmDB as ImmDB
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Iterator as Iterator
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.LgrDB as LgrDB
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Reader as Reader
import           Ouroboros.Consensus.Storage.ChainDB.Impl.Types
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.VolDB as VolDB

isOpen :: IOLike m => ChainDbHandle m blk -> STM m Bool
isOpen (CDBHandle varState) = readTVar varState <&> \case
    ChainDbReopening   -> False
    ChainDbClosed _env -> False
    ChainDbOpen   _env -> True

closeDB
  :: forall m blk.
     ( IOLike m
     , HasHeader (Header blk)
     , HasCallStack
     )
  => ChainDbHandle m blk -> m ()
closeDB (CDBHandle varState) = do
    mbOpenEnv <- atomically $ readTVar varState >>= \case
      -- Wait until reopening the ChainDB finished
      ChainDbReopening   -> retry
      -- Idempotent
      ChainDbClosed _env -> return Nothing
      ChainDbOpen    env -> do
        writeTVar varState $ ChainDbClosed env
        return $ Just env

    -- Only when the ChainDB was open
    whenJust mbOpenEnv $ \cdb@CDB{..} -> do

      Reader.closeAllReaders     cdb
      Iterator.closeAllIterators cdb

      killBgThreads <- atomically $ readTVar cdbKillBgThreads
      killBgThreads

      ImmDB.closeDB cdbImmDB
      VolDB.closeDB cdbVolDB

      chain <- atomically $ readTVar cdbChain

      traceWith cdbTracer $ TraceOpenEvent $ ClosedDB
        { _immTip   = castPoint $ AF.anchorPoint chain
        , _chainTip = castPoint $ AF.headPoint chain
        }

reopen
  :: forall m blk.
     ( IOLike m
     , ProtocolLedgerView blk
     , HasCallStack
     )
  => ChainDbHandle m blk
  -> Bool -- ^ 'True' = Launch background tasks
  -> m ()
reopen (CDBHandle varState) launchBgTasks = do
    mbClosedEnv <- atomically $ readTVar varState >>= \case
      -- Another call to 'cdbReopen' is in progress. Wait until it is
      -- finished, then try reopening if it is still necessary.
      ChainDbReopening  -> retry
      -- No-op
      ChainDbOpen  _env -> return Nothing
      ChainDbClosed env -> do
        writeTVar varState ChainDbReopening
        return $ Just env

    -- Only when the ChainDB was closed
    whenJust mbClosedEnv $ \env@CDB{..} ->
      -- When something goes wrong, reset the state to 'ChainDbClosed'
      flip onException (atomically $ writeTVar varState $ ChainDbClosed env) $ do
        -- TODO what will actually happen if an exception is thrown? What if
        -- recovery is triggered?

        ImmDB.reopen cdbImmDB
        immDbTipPoint <- ImmDB.getPointAtTip cdbImmDB
        immDbTipEpoch <- pointToEpoch cdbEpochInfo immDbTipPoint
        traceWith cdbTracer $ TraceOpenEvent $ OpenedImmDB
          { _immDbTip      = immDbTipPoint
          , _immDbTipEpoch = immDbTipEpoch
          }

        -- Note that we must reopen the VolatileDB before the LedgerDB, as the
        -- latter may try to access the former: when we initially opened it,
        -- we passed it @getAnyKnownBlock immDB volDB@, which will be called
        -- during reopening.
        VolDB.reopen cdbVolDB
        traceWith cdbTracer $ TraceOpenEvent OpenedVolDB

        lgrReplayTracer <- LgrDB.decorateReplayTracer
          cdbEpochInfo
          immDbTipPoint
          (contramap TraceLedgerReplayEvent cdbTracer)
        replayed <- LgrDB.reopen cdbLgrDB cdbImmDB lgrReplayTracer
        traceWith cdbTracer $ TraceOpenEvent OpenedLgrDB

        curSlot        <- atomically $ getCurrentSlot cdbBlockchainTime
        chainAndLedger <- initialChainSelection
           cdbImmDB
           cdbVolDB
           cdbLgrDB
           cdbTracer
           cdbNodeConfig
           cdbInvalid
           curSlot

        let chain      = clChain  chainAndLedger
            ledger     = clLedger chainAndLedger

        atomically $ do
          writeTVar cdbChain chain
          LgrDB.setCurrent cdbLgrDB ledger
          -- Change the state from 'ChainDbReopening' to 'ChainDbOpen'
          writeTVar varState $ ChainDbOpen env
        traceWith cdbTracer $ TraceOpenEvent $ ReopenedDB
          { _immTip   = castPoint $ AF.anchorPoint chain
          , _chainTip = castPoint $ AF.headPoint   chain
          }

        when launchBgTasks $ Background.launchBgTasks env replayed


{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

pointToEpoch :: Monad m => EpochInfo m -> Point blk -> m EpochNo
pointToEpoch epochInfo = \case
    GenesisPoint      -> return 0
    BlockPoint slot _ -> epochInfoEpoch epochInfo slot
