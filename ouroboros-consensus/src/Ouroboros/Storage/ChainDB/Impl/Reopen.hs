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

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer

import           Control.Tracer

import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (HasHeader (..), castPoint)
import           Ouroboros.Network.Chain (genesisBlockNo)

import           Ouroboros.Consensus.Block (Header)
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util (whenJust)

import qualified Ouroboros.Storage.ChainDB.Impl.Background as Background
import           Ouroboros.Storage.ChainDB.Impl.ChainSel
import qualified Ouroboros.Storage.ChainDB.Impl.ImmDB as ImmDB
import qualified Ouroboros.Storage.ChainDB.Impl.Iterator as Iterator
import qualified Ouroboros.Storage.ChainDB.Impl.LgrDB as LgrDB
import qualified Ouroboros.Storage.ChainDB.Impl.Reader as Reader
import           Ouroboros.Storage.ChainDB.Impl.Types
import qualified Ouroboros.Storage.ChainDB.Impl.VolDB as VolDB

isOpen :: MonadSTM m => ChainDbHandle m blk -> STM m Bool
isOpen (CDBHandle varState) = readTVar varState <&> \case
    ChainDbReopening   -> False
    ChainDbClosed _env -> False
    ChainDbOpen   _env -> True

closeDB
  :: forall m blk.
     ( MonadCatch m
     , MonadAsync m
     , HasHeader blk
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
      mapM_ cancel bgThreads

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
     ( MonadAsync m
     , MonadFork  m
     , MonadMask  m
     , MonadST    m
     , MonadTime  m
     , MonadTimer m
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
        -- Note that we must reopen the VolatileDB before the LedgerDB, as the
        -- latter may try to access the former: when we initially opened it,
        -- we passed it @getAnyKnownBlock immDB volDB@, which will be called
        -- during reopening.
        VolDB.reopen cdbVolDB
        let lgrTracer = contramap (TraceLedgerEvent . InitLog) cdbTracer
        LgrDB.reopen cdbLgrDB cdbImmDB lgrTracer

        chainAndLedger <- initialChainSelection
           cdbImmDB
           cdbVolDB
           cdbLgrDB
           cdbTracer
           cdbNodeConfig
           cdbInvalid

        -- Get the actual BlockNo of the tip of the ImmutableDB. Note that
        -- this might not end up being the \"immutable\" block(no), because
        -- the current chain computed from the VolatileDB could be longer than
        -- @k@.
        immDbBlockNo <- maybe genesisBlockNo blockNo <$>
          ImmDB.getBlockAtTip cdbImmDB

        let chain      = clChain  chainAndLedger
            ledger     = clLedger chainAndLedger
            secParam   = protocolSecurityParam cdbNodeConfig
            immBlockNo = getImmBlockNo secParam chain immDbBlockNo

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
