{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wredundant-constraints #-}

-- | Closing and reopening
module Ouroboros.Storage.ChainDB.Impl.Reopen
  ( isOpen
  , closeDB
  , reopen
  ) where

import           Control.Monad (when)
import           Data.Functor ((<&>))
import           GHC.Stack (HasCallStack)

import           Control.Monad.Class.MonadThrow
import           Control.Tracer

import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (HasHeader (..), castPoint,
                     genesisBlockNo, genesisPoint)

import           Ouroboros.Consensus.Block (Header, headerPoint)
import           Ouroboros.Consensus.BlockchainTime (getCurrentSlot)
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util (whenJust)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry

import           Ouroboros.Storage.Common (EpochNo)
import           Ouroboros.Storage.EpochInfo (epochInfoEpoch)

import           Ouroboros.Storage.ChainDB.API (BlockOrHeader (..))
import qualified Ouroboros.Storage.ChainDB.Impl.Background as Background
import           Ouroboros.Storage.ChainDB.Impl.ChainSel
import qualified Ouroboros.Storage.ChainDB.Impl.ImmDB as ImmDB
import qualified Ouroboros.Storage.ChainDB.Impl.Iterator as Iterator
import qualified Ouroboros.Storage.ChainDB.Impl.LgrDB as LgrDB
import qualified Ouroboros.Storage.ChainDB.Impl.Reader as Reader
import           Ouroboros.Storage.ChainDB.Impl.Types
import qualified Ouroboros.Storage.ChainDB.Impl.VolDB as VolDB

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

      bgThreads <- atomically $ readTVar cdbBgThreads
      mapM_ cancelThread bgThreads

      -- TODO Maybe write a 'LedgerDB' snapshot or wait until it is done.
      -- See #367.
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

        let blockEpoch :: forall b. HasHeader b => b -> m EpochNo
            blockEpoch = epochInfoEpoch cdbEpochInfo . blockSlot

        ImmDB.reopen cdbImmDB
        -- In order to figure out the 'BlockNo' and 'Point' at the tip of the
        -- ImmutableDB, we need to read the header at the tip of the ImmutableDB.
        immDbTipHeader <- ImmDB.getBlockOrHeaderAtTip cdbImmDB Header
        -- Note that 'immDbTipBlockNo' might not end up being the \"immutable\"
        -- block(no), because the current chain computed from the VolatileDB could
        -- be longer than @k@.
        let immDbTipBlockNo = maybe genesisBlockNo blockNo     immDbTipHeader
            immDbTipPoint   = maybe genesisPoint   headerPoint immDbTipHeader
        immDbTipEpoch      <- maybe (return 0)     blockEpoch  immDbTipHeader
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
        LgrDB.reopen cdbLgrDB cdbImmDB lgrReplayTracer
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
            secParam   = protocolSecurityParam cdbNodeConfig
            immBlockNo = getImmBlockNo secParam chain immDbTipBlockNo

        atomically $ do
          writeTVar cdbChain chain
          LgrDB.setCurrent cdbLgrDB ledger
          writeTVar cdbImmBlockNo immBlockNo
          -- Change the state from 'ChainDbReopening' to 'ChainDbOpen'
          writeTVar varState $ ChainDbOpen env
        traceWith cdbTracer $ TraceOpenEvent $ ReopenedDB
          { _immTip   = castPoint $ AF.anchorPoint chain
          , _chainTip = castPoint $ AF.headPoint   chain
          }

        when launchBgTasks $ Background.launchBgTasks env
