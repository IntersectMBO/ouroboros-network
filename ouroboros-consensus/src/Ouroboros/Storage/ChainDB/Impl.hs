{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wredundant-constraints #-}

module Ouroboros.Storage.ChainDB.Impl (
    -- * Initialization
    ChainDbArgs(..)
  , defaultArgs
  , openDB
    -- * Trace types
  , TraceEvent (..)
  , TraceAddBlockEvent (..)
  , TraceReaderEvent (..)
  , TraceCopyToImmDBEvent (..)
  , TraceGCEvent (..)
  , TraceValidationEvent (..)
  , TraceInitChainSelEvent (..)
  , TraceOpenEvent (..)
  , TraceIteratorEvent (..)
  , TraceLedgerEvent (..)
  , ReasonInvalid (..)
    -- * Internals for testing purposes
  , openDBInternal
  , Internal (..)
  ) where

import           Control.Monad (when)
import qualified Data.Map as Map
import qualified Data.Set as Set

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer

import           Control.Tracer

import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (blockNo, castPoint)
import           Ouroboros.Network.Chain (genesisBlockNo)

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.Abstract

import           Ouroboros.Storage.ChainDB.API

import           Ouroboros.Storage.ChainDB.Impl.Args (ChainDbArgs, defaultArgs)
import qualified Ouroboros.Storage.ChainDB.Impl.Args as Args
import qualified Ouroboros.Storage.ChainDB.Impl.Background as Background
import qualified Ouroboros.Storage.ChainDB.Impl.ChainSel as ChainSel
import qualified Ouroboros.Storage.ChainDB.Impl.ImmDB as ImmDB
import qualified Ouroboros.Storage.ChainDB.Impl.Iterator as Iterator
import qualified Ouroboros.Storage.ChainDB.Impl.LgrDB as LgrDB
import qualified Ouroboros.Storage.ChainDB.Impl.Query as Query
import qualified Ouroboros.Storage.ChainDB.Impl.Reader as Reader
import qualified Ouroboros.Storage.ChainDB.Impl.Reopen as Reopen
import           Ouroboros.Storage.ChainDB.Impl.Types
import qualified Ouroboros.Storage.ChainDB.Impl.VolDB as VolDB

{-------------------------------------------------------------------------------
  Initialization
-------------------------------------------------------------------------------}

openDB
  :: forall m blk.
     ( MonadAsync m
     , MonadFork  m
     , MonadST    m
     , MonadMask  m
     , MonadTime  m
     , MonadTimer m
     , MonadThrow (STM m)
     , ProtocolLedgerView blk
     )
  => ChainDbArgs m blk
  -> m (ChainDB m blk)
openDB args = fst <$> openDBInternal args True

openDBInternal
  :: forall m blk.
     ( MonadAsync m
     , MonadFork  m
     , MonadST    m
     , MonadMask  m
     , MonadTime  m
     , MonadTimer m
     , MonadThrow (STM m)
     , ProtocolLedgerView blk
     )
  => ChainDbArgs m blk
  -> Bool -- ^ 'True' = Launch background tasks
  -> m (ChainDB m blk, Internal m blk)
openDBInternal args launchBgTasks = do
    immDB   <- ImmDB.openDB argsImmDb
    volDB   <- VolDB.openDB argsVolDb
    lgrDB   <- LgrDB.openDB argsLgrDb
                            immDB
                            (Query.getAnyKnownBlock immDB volDB)
                            lgrTracer

    varInvalid <- atomically $ newTVar Set.empty

    chainAndLedger <- ChainSel.initialChainSelection
      immDB
      volDB
      lgrDB
      (Args.cdbTracer     args)
      (Args.cdbNodeConfig args)
      varInvalid

    -- Get the actual BlockNo of the tip of the ImmutableDB. Note that this
    -- might not end up being the \"immutable\" block(no), because the current
    -- chain computed from the VolatileDB could be longer than @k@.
    immDbBlockNo <- maybe genesisBlockNo blockNo <$> ImmDB.getBlockAtTip immDB

    let chain      = ChainSel.clChain  chainAndLedger
        ledger     = ChainSel.clLedger chainAndLedger
        cfg        = Args.cdbNodeConfig args
        secParam   = protocolSecurityParam cfg
        immBlockNo = ChainSel.getImmBlockNo secParam chain immDbBlockNo

    atomically $ LgrDB.setCurrent lgrDB ledger
    varChain          <- atomically $ newTVar chain
    varImmBlockNo     <- atomically $ newTVar immBlockNo
    varIterators      <- atomically $ newTVar Map.empty
    varReaders        <- atomically $ newTVar Map.empty
    varNextIteratorId <- atomically $ newTVar $ IteratorId 0
    varNextReaderId   <- atomically $ newTVar 0
    varCopyLock       <- atomically $ newTMVar ()
    varBgThreads      <- atomically $ newTVar []

    let env = CDB { cdbImmDB          = immDB
                  , cdbVolDB          = volDB
                  , cdbLgrDB          = lgrDB
                  , cdbChain          = varChain
                  , cdbImmBlockNo     = varImmBlockNo
                  , cdbIterators      = varIterators
                  , cdbReaders        = varReaders
                  , cdbNodeConfig     = cfg
                  , cdbInvalid        = varInvalid
                  , cdbNextIteratorId = varNextIteratorId
                  , cdbNextReaderId   = varNextReaderId
                  , cdbCopyLock       = varCopyLock
                  , cdbTracer         = Args.cdbTracer         args
                  , cdbThreadRegistry = Args.cdbThreadRegistry args
                  , cdbGcDelay        = Args.cdbGcDelay        args
                  , cdbBgThreads      = varBgThreads
                  }
    h <- fmap CDBHandle $ atomically $ newTVar $ ChainDbOpen env
    let chainDB = ChainDB
          { addBlock           = getEnv1    h ChainSel.addBlock
          , getCurrentChain    = getEnvSTM  h Query.getCurrentChain
          , getCurrentLedger   = getEnvSTM  h Query.getCurrentLedger
          , getTipBlock        = getEnv     h Query.getTipBlock
          , getTipHeader       = getEnv     h Query.getTipHeader
          , getTipPoint        = getEnvSTM  h Query.getTipPoint
          , getBlock           = getEnv1    h Query.getBlock
          , getIsFetched       = getEnvSTM  h Query.getIsFetched
          , streamBlocks       = Iterator.streamBlocks  h
          , newHeaderReader    = Reader.newHeaderReader h
          , newBlockReader     = Reader.newBlockReader  h
          , knownInvalidBlocks = getEnvSTM  h Query.knownInvalidBlocks
          , closeDB            = Reopen.closeDB h
          , isOpen             = Reopen.isOpen  h
          }
        testing = Internal
          { intReopen                = Reopen.reopen  h
          , intCopyToImmDB           = getEnv  h Background.copyToImmDB
          , intGarbageCollect        = getEnv1 h Background.garbageCollect
          , intUpdateLedgerSnapshots = getEnv  h Background.updateLedgerSnapshots
          , intBgThreads             = varBgThreads
          }

    traceWith (Args.cdbTracer args) $ TraceOpenEvent $ OpenedDB
      { _immTip   = castPoint $ AF.anchorPoint chain
      , _chainTip = castPoint $ AF.headPoint   chain
      }

    when launchBgTasks $ Background.launchBgTasks env

    return (chainDB, testing)
  where
    (argsImmDb, argsVolDb, argsLgrDb, _) = Args.fromChainDbArgs args

    lgrTracer = contramap (TraceLedgerEvent . InitLog) (Args.cdbTracer args)
